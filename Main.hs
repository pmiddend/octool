{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative ((<*>), optional, pure)
import Control.Monad ((>>), void, when)
import Data.Bool (Bool(..), (&&), (||), not, otherwise)
import Data.ByteString (pack, readFile)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (Either(..))
import Data.Eq (Eq, (==))
import Data.Foldable (foldMap, forM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Contravariant (Predicate(..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List
  ( (!!)
  , elem
  , filter
  , head
  , init
  , intercalate
  , isPrefixOf
  , length
  , lines
  , nub
  , unwords
  )
import Data.Maybe (Maybe(..), fromJust, isJust, listToMaybe, mapMaybe)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.String (String)
import Debug.Trace (trace)
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
import System.Directory (copyFile, doesFileExist, findExecutable)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.FilePath (FilePath, (</>), splitPath)
import System.IO (Handle, IO, getLine, hGetLine, hIsEOF, putStrLn)
import System.Process (callCommand)
import System.Process
  ( ProcessHandle
  , StdStream(CreatePipe)
  , callProcess
  , createProcess
  , proc
  , readProcess
  , spawnProcess
  , std_out
  , waitForProcess
  )
import Text.Show (show)
import Xeno.DOM (Content(..), children, contents, name, parse)

data Opts =
  Opts
    { optsRelativeTo :: Maybe String
    , optsRebuildSome :: Maybe String
    , optsCreateCa :: Maybe String
    , optsClean :: Bool
    , optsStdout :: Bool
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
    (strOption (long "rebuild-some" <> help "rebuild only specific modules")) <*>
  optional
    (strOption
       (long "create-ca" <>
        help "create a capture agent on the locally running instance")) <*>
  switch (long "clean" <> help "clean before build") <*>
  switch (long "stdout" <> help "output stdout, too") <*>
  switch (long "yes" <> help "say \"yes\" to everything") <*>
  switch (long "no" <> help "say \"no\" to everything") <*>
  switch (long "no-checkstyle" <> help "disable checkstyle") <*>
  switch (long "no-tests" <> help "disable tests")

optStr :: Bool -> String -> [String]
optStr True x = [x]
optStr False _ = []

notifySend :: String -> IO ()
notifySend x = callProcess "notify-send" [x]

safefify p f = do
  exe <- findExecutable p
  case exe of
    Nothing -> error ("Couldn't find \"" <> p <> "\" in PATH")
    Just exe' -> f exe'

safeSpawnProcess :: String -> [String] -> IO ProcessHandle
safeSpawnProcess p args =
  safefify p $ \command -> do
    putStrLn $ "executing " <> command <> " " <> unwords args
    spawnProcess command args

safeReadProcess p args input =
  safefify p $ \command -> readProcess command args input

mvn :: [String] -> IO () -> IO ()
mvn args f = do
  handle <- safeSpawnProcess "mvn" args
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> f
    ExitFailure i -> notifySend ("mvn error (code " <> show i <> ")")

parseSafe f = do
  c <- readFile f
  case parse c of
    Left e -> error $ "error parsing \"" <> f <> "\": " <> show e
    Right v -> pure v

nodeContent n = foldMap contentToText (contents n)
  where
    contentToText (Text x) = x
    contentToText _ = ""

getVersion = do
  pom <- parseSafe "pom.xml"
  case listToMaybe (filter ((== "version") . name) (children pom)) of
    Nothing -> error "couldn't find \"version\" in pom.xml"
    Just version -> pure (BS8.unpack (nodeContent version))

prompt opts prompt
  | optsYes opts = pure True
  | optsNo opts = pure False
  | otherwise = do
    putStrLn (prompt <> " [yn]")
    line <- getLine
    case line of
      "y" -> pure True
      "n" -> pure False

rebuildSome mod opts =
  let realMod = "modules/" <> mod
   in mvnPretty (optsStdout opts) (mvnOpts opts <> ["install", "-pl", realMod]) $ do
        copyModule mod
        notifySend $ "rebuild \"" <> mod <> "\" succeeded!"

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

partialRebuild relativeTo opts = do
  (rebuildType, mods) <- changedModules relativeTo
  fullRebuild' <-
    if rebuildType == FullRebuild
      then prompt opts "\"pom.xml\" changed, do a full rebuild?"
      else pure False
  if fullRebuild'
    then fullRebuild opts
    else mvnPretty
           (optsStdout opts)
           (mvnOpts opts <>
            ["install", "-pl", intercalate "," (("modules" </>) <$> mods)]) $ do
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

mvnOpts opts =
  ["-B"] <>
  optStr (optsNoTests opts) "-DskipTests" <>
  optStr (optsNoCheckstyle opts) "-Dcheckstyle.skip" <>
  optStr (optsClean opts) "clean"

fullRebuild opts =
  mvnPretty
    (optsStdout opts)
    (mvnOpts opts <> ["package", "install", "-Pdev"])
    (notifySend "full rebuild success")

mvnPretty :: Bool -> [String] -> IO () -> IO ()
mvnPretty stdout args f = do
  putStrLn ("mvn " <> intercalate " " args)
  (_, Just hout, _, processHandle) <-
    createProcess ((proc "mvn" args) {std_out = CreatePipe})
  errorLines <- newIORef []
  errorCode <- mvnWithParse' hout processHandle errorLines
  case errorCode of
    ExitSuccess -> f
    ExitFailure i -> notifySend $ "failure (code " <> show i <> ")!"
  where
    mvnWithParse' :: Handle -> ProcessHandle -> IORef [String] -> IO ExitCode
    mvnWithParse' h p errLines = do
      eof <- hIsEOF h
      if eof
        then waitForProcess p
        else do
          line <- hGetLine h
          let lineError :: Bool
              lineError = "[ERROR]" `isPrefixOf` line
              lineUpdate :: Bool
              lineUpdate = "[INFO] Building" `isPrefixOf` line
          when lineError (putStrLn line >> modifyIORef errLines (line :))
          when (lineUpdate || (stdout && not lineError)) (putStrLn line)
          mvnWithParse' h p errLines

createCa :: String -> IO ()
createCa caName =
  callCommand $
  "curl -i -s -f --digest -u opencast_system_account:CHANGE_ME --request POST -H \"X-Requested-Auth: Digest\" --data state=idle 'http://localhost:8080/capture-admin/agents/" <>
  caName <> "'"

main :: IO ()
main = do
  opts <- execParser (info (optsParser <**> helper) fullDesc)
  case optsCreateCa opts of
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
