{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative ((<*>), optional, pure)
import Control.Monad ((>=>), (>>), when)
import Data.Bool (Bool(..), (&&), (||), not, otherwise)
import Data.ByteString (readFile)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (Either(..))
import Data.Eq (Eq, (==))
import Data.Foldable (foldMap, forM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.IORef (IORef, modifyIORef, newIORef)
import Data.List
  ( (!!)
  , elem
  , filter
  , init
  , intercalate
  , isPrefixOf
  , length
  , lines
  , nub
  , unwords
  , words
  )
import Data.Maybe (Maybe(..), isJust, listToMaybe, mapMaybe)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.String (String)
import Options.Applicative
  ( Parser
  , (<**>)
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , strOption
  , switch
  )
import Prelude (error, print)
import System.Directory (copyFile, findExecutable)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.FilePath (FilePath, (</>), splitPath)
import System.IO
  ( Handle
  , IO
  , IOMode(ReadMode, WriteMode)
  , getLine
  , hGetContents
  , hGetLine
  , hIsEOF
  , hPutStrLn
  , putStrLn
  , withFile
  )
import System.Process
  ( ProcessHandle
  , StdStream(CreatePipe)
  , callCommand
  , callProcess
  , createProcess
  , proc
  , readProcess
  , std_out
  , waitForProcess
  )
import Text.Show (show)
import Xeno.DOM (Content(..), Node, children, contents, name, parse)

data Opts =
  Opts
    { optsRelativeTo :: Maybe String
    , optsRebuildSome :: Maybe [String]
    , optsCreateCa :: Maybe String
    , optsClean :: Bool
    , optsStdout :: Bool
    , optsShowLastLog :: Bool
    , optsYes :: Bool
    , optsNo :: Bool
    , optsNoCheckstyle :: Bool
    , optsNoTests :: Bool
    }

optsParser :: Parser Opts
optsParser =
  Opts <$>
  optional
    (strOption
       (long "relative-to" <>
        help "rebuild only modules changed since the version specified")) <*>
  optional
    (words <$>
     strOption
       (long "rebuild" <>
        help "rebuild this specific modules (and dependents thereof)")) <*>
  optional
    (strOption
       (long "create-ca" <>
        help "create a capture agent on the locally running instance")) <*>
  switch (long "clean" <> help "clean before build") <*>
  switch (long "stdout" <> help "output stdout, too") <*>
  switch (long "show-last-log" <> help "show last log") <*>
  switch (long "yes" <> help "say \"yes\" to everything") <*>
  switch (long "no" <> help "say \"no\" to everything") <*>
  switch (long "no-checkstyle" <> help "disable checkstyle") <*>
  switch (long "no-tests" <> help "disable tests")

optStr :: Bool -> String -> [String]
optStr True x = [x]
optStr False _ = []

notifySend :: String -> IO ()
notifySend x = callProcess "notify-send" [x]

safefify :: String -> (FilePath -> IO b) -> IO b
safefify p f = do
  exe <- findExecutable p
  case exe of
    Nothing -> error ("Couldn't find \"" <> p <> "\" in PATH")
    Just exe' -> f exe'

safeReadProcess :: String -> [String] -> String -> IO String
safeReadProcess p args input =
  safefify p $ \command -> readProcess command args input

data MvnResult
  = MvnResultOk
  | MvnResultFailure

parseSafe :: FilePath -> IO Node
parseSafe f = do
  c <- readFile f
  case parse c of
    Left e -> error $ "error parsing \"" <> f <> "\": " <> show e
    Right v -> pure v

nodeContent :: Node -> BS8.ByteString
nodeContent n = foldMap contentToText (contents n)
  where
    contentToText (Text x) = x
    contentToText _ = ""

getVersion :: IO String
getVersion = do
  pom <- parseSafe "pom.xml"
  case listToMaybe (filter ((== "version") . name) (children pom)) of
    Nothing -> error "couldn't find \"version\" in pom.xml"
    Just version -> pure (BS8.unpack (nodeContent version))

prompt :: Opts -> String -> IO Bool
prompt opts promptStr
  | optsYes opts = pure True
  | optsNo opts = pure False
  | otherwise = do
    putStrLn (promptStr <> " [yn]")
    line <- getLine
    case line of
      "y" -> pure True
      "n" -> pure False
      _ -> prompt opts promptStr

rebuildSome :: [String] -> Opts -> IO ()
rebuildSome mods opts = do
  let modPaths = intercalate "," (("modules/" <>) <$> mods)
  result <-
    mvnPretty
      (optsStdout opts)
      (mvnOpts opts <> ["install", "--projects", modPaths])
  whenSuccess result $
    forM_ mods $ \mod -> do
      copyModule mod
      notifySend $ "rebuild \"" <> unwords mods <> "\" succeeded!"

copyModule :: FilePath -> IO ()
copyModule mod = do
  home <- getEnv "HOME"
  version <- getVersion
  let ocPath :: FilePath
      ocPath =
        "org" </> "opencastproject" </> ("opencast-" <> mod) </> version </>
        ("opencast-" <> mod <> "-" <> version <> ".jar")
      from :: FilePath
      from = home </> ".m2" </> "repository" </> ocPath
      to :: FilePath
      to =
        "build" </> ("opencast-dist-develop-" <> version) </> "system" </>
        ocPath
  putStrLn $ "copying " <> from <> " to " <> to
  copyFile from to

partialRebuild :: String -> Opts -> IO ()
partialRebuild relativeTo opts = do
  (rebuildType, mods) <- changedModules relativeTo
  fullRebuild' <-
    if rebuildType == FullRebuild
      then prompt opts "\"pom.xml\" changed, do a full rebuild?"
      else pure False
  if fullRebuild'
    then fullRebuild opts
    else do
      result <-
        mvnPretty
          (optsStdout opts)
          (mvnOpts opts <>
           ["install", "-pl", intercalate "," (("modules" </>) <$> mods)])
      whenSuccess result $ do
        print mods
        forM_ mods copyModule
        notifySend "Partial rebuild complete"

gitDiff :: String -> IO [String]
gitDiff relativeTo =
  lines <$> safeReadProcess "git" ["diff", "--name-only", relativeTo] ""

data RebuildType
  = FullRebuild
  | PartialRebuild
  deriving (Eq)

changedModules :: String -> IO (RebuildType, [String])
changedModules relativeTo = do
  gitResult <- gitDiff relativeTo
  --let takeSecond p | length p > 1 = Just (init (head p <> (p !! 1)))
  let takeSecond p
        | length p > 1 = Just (init (p !! 1))
        | otherwise = Nothing
      modules =
        nub $
        mapMaybe takeSecond $
        splitPath <$> filter ("modules" `isPrefixOf`) gitResult
  pure
    ( if "pom.xml" `elem` gitResult
        then FullRebuild
        else PartialRebuild
    , modules)

mvnOpts :: Opts -> [String]
mvnOpts opts =
  ["--batch-mode"] <>
  optStr (optsNoTests opts) "-DskipTests" <>
  optStr (optsNoCheckstyle opts) "-Dcheckstyle.skip" <>
  optStr (optsClean opts) "clean"

whenSuccess :: MvnResult -> IO () -> IO ()
whenSuccess MvnResultOk f = f
whenSuccess _ _ = pure ()

fullRebuild :: Opts -> IO ()
fullRebuild opts = do
  result <-
    mvnPretty
      (optsStdout opts)
      (mvnOpts opts <> ["package", "install", "-Pdev"])
  whenSuccess result (notifySend "full rebuild success")

lastLogFileName :: FilePath
lastLogFileName = "last-log.txt"

mvnPretty :: Bool -> [String] -> IO MvnResult
mvnPretty stdout args = do
  putStrLn ("mvn " <> unwords args)
  (_, Just hout, _, processHandle) <-
    createProcess ((proc "mvn" args) {std_out = CreatePipe})
  errorLines <- newIORef []
  withFile lastLogFileName WriteMode $ \lastLog -> do
    errorCode <- mvnWithParse' hout lastLog processHandle errorLines
    case errorCode of
      ExitSuccess -> pure MvnResultOk
      ExitFailure i -> do
        notifySend $ "failure (code " <> show i <> ")!"
        pure MvnResultFailure
  where
    mvnWithParse' ::
         Handle -> Handle -> ProcessHandle -> IORef [String] -> IO ExitCode
    mvnWithParse' h lastLog p errLines = do
      eof <- hIsEOF h
      if eof
        then waitForProcess p
        else do
          line <- hGetLine h
          hPutStrLn lastLog line
          let lineError :: Bool
              lineError = "[ERROR]" `isPrefixOf` line
              lineUpdate :: Bool
              lineUpdate = "[INFO] Building" `isPrefixOf` line
          when lineError (putStrLn line >> modifyIORef errLines (line :))
          when (lineUpdate || (stdout && not lineError)) (putStrLn line)
          mvnWithParse' h lastLog p errLines

createCa :: String -> IO ()
createCa caName =
  callCommand $
  "curl -i -s -f --digest -u opencast_system_account:CHANGE_ME --request POST -H \"X-Requested-Auth: Digest\" --data state=idle 'http://localhost:8080/capture-admin/agents/" <>
  caName <> "'"

main :: IO ()
main = do
  opts <- execParser (info (optsParser <**> helper) fullDesc)
  if optsShowLastLog opts
    then withFile lastLogFileName ReadMode (hGetContents >=> putStrLn)
    else case optsCreateCa opts of
           Just caName -> createCa caName
           Nothing -> do
             when
               (isJust (optsRebuildSome opts) && isJust (optsRelativeTo opts))
               (error "can't specify some rebuild and relative rebuild")
             case optsRebuildSome opts of
               Just x -> rebuildSome x opts
               _ ->
                 case optsRelativeTo opts of
                   Just x -> partialRebuild x opts
                   _ -> fullRebuild opts
