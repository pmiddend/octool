{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative ((<*>), optional, pure)
import Control.Monad (Monad, (>=>), (>>), (>>=), when)
import Data.Bool (Bool(..), (&&), (||), not, otherwise)
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (Either(..))
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (foldMap, forM_)
import Data.Function (($), (&), (.), id)
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
import Data.Maybe (Maybe(..), fromMaybe, isJust, listToMaybe, mapMaybe)
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
import Polysemy (Embed, Member, Members, Sem, embed, interpret, makeSem, runM)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Fail (Fail, failToError)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.Resource (Resource, bracket, resourceToIO)
import Prelude (error)
import System.Directory (copyFile, findExecutable)
import System.Environment (getEnv)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (FilePath, (</>), splitPath)
import System.IO
  ( Handle
  , IO
  , IOMode(ReadMode, WriteMode)
  , getLine
  , hClose
  , hGetContents
  , hGetLine
  , hIsEOF
  , hPutStrLn
  , openFile
  , putStrLn
  , withFile
  )
import System.Process
  ( CreateProcess
  , ProcessHandle
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
import Xeno.Types (XenoException)

type Assembly = String

data Opts =
  Opts
    { optsRelativeTo :: Maybe String
    , optsRebuildSome :: Maybe [String]
    , optsCreateCa :: Maybe String
    , optsTargetAssembly :: Maybe Assembly
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
  optional
    (strOption
       (long "target-assembly" <>
        help "use the specified assembly instead of develop")) <*>
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

data LogSemantics m a where
  MyLog :: String -> LogSemantics m ()

makeSem ''LogSemantics

runLog :: Member (Embed IO) r => Sem (LogSemantics : r) a -> Sem r a
runLog =
  interpret $ \case
    MyLog s -> embed (putStrLn s)

data PromptSemantics m a where
  MyPrompt :: String -> PromptSemantics m Bool

makeSem ''PromptSemantics

runInteractivePrompt ::
     Member (Embed IO) r => Sem (PromptSemantics : r) a -> Sem r a
runInteractivePrompt =
  interpret $ \case
    MyPrompt promptStr' -> f promptStr'
      where f promptStr = do
              embed (putStrLn (promptStr <> " [yn]"))
              line <- embed getLine
              case line of
                "y" -> pure True
                "n" -> pure False
                _ -> f promptStr

runConstantPrompt :: Bool -> Sem (PromptSemantics : r) a -> Sem r a
runConstantPrompt b =
  interpret $ \case
    MyPrompt _ -> pure b

runPrompt ::
     Member (Embed IO) r => Opts -> Sem (PromptSemantics : r) a -> Sem r a
runPrompt opts
  | optsYes opts = runConstantPrompt True
  | optsNo opts = runConstantPrompt False
  | otherwise = runInteractivePrompt

data FileSemantics m a where
  MyReadFile :: FilePath -> FileSemantics m ByteString
  MyNewIORef :: a -> FileSemantics m (IORef a)
  MyHClose :: Handle -> FileSemantics m ()
  MyOpenFile :: FilePath -> IOMode -> FileSemantics m Handle
  MyModifyIORef :: IORef a -> (a -> a) -> FileSemantics m ()
  MyHIsEOF :: Handle -> FileSemantics m Bool
  MyHGetLine :: Handle -> FileSemantics m String
  MyHPutStrLn :: Handle -> String -> FileSemantics m ()
  MyGetEnv :: String -> FileSemantics m String
  MyCopyFile :: FilePath -> FilePath -> FileSemantics m ()

makeSem ''FileSemantics

runFileSemantics :: Member (Embed IO) r => Sem (FileSemantics : r) a -> Sem r a
runFileSemantics =
  interpret $ \case
    MyReadFile fp -> embed (readFile fp)
    MyNewIORef a -> embed (newIORef a)
    MyHClose h -> embed (hClose h)
    MyOpenFile fp mode -> embed (openFile fp mode)
    MyModifyIORef ref f -> embed (modifyIORef ref f)
    MyHIsEOF h -> embed (hIsEOF h)
    MyHGetLine h -> embed (hGetLine h)
    MyHPutStrLn h s -> embed (hPutStrLn h s)
    MyGetEnv h -> embed (getEnv h)
    MyCopyFile s d -> embed (copyFile s d)

data XmlSemantics m a where
  MyParseString :: ByteString -> XmlSemantics m (Either XenoException Node)

makeSem ''XmlSemantics

runXml :: Sem (XmlSemantics : r) a -> Sem r a
runXml =
  interpret $ \case
    MyParseString s -> pure (parse s)

data ProcessSemantics m a where
  MyReadProcess :: String -> [String] -> String -> ProcessSemantics m String
  MyCallProcess :: FilePath -> [String] -> ProcessSemantics m ()
  MyCallCommand :: String -> ProcessSemantics m ()
  MyFindExecutable :: String -> ProcessSemantics m (Maybe FilePath)
  MyWaitForProcess :: ProcessHandle -> ProcessSemantics m ExitCode
  MyCreateProcess
    :: CreateProcess
    -> ProcessSemantics m ( Maybe Handle
                          , Maybe Handle
                          , Maybe Handle
                          , ProcessHandle)

makeSem ''ProcessSemantics

runProcessSemantics ::
     Member (Embed IO) r => Sem (ProcessSemantics : r) a -> Sem r a
runProcessSemantics =
  interpret $ \case
    MyReadProcess s args input -> embed (readProcess s args input)
    MyCallProcess f args -> embed (callProcess f args)
    MyCallCommand s -> embed (callCommand s)
    MyFindExecutable f -> embed (findExecutable f)
    MyWaitForProcess h -> embed (waitForProcess h)
    MyCreateProcess cp -> embed (createProcess cp)

notifySend :: Member ProcessSemantics r => String -> Sem r ()
notifySend x = myCallProcess "notify-send" [x]

safefify ::
     Members '[ ProcessSemantics, Error String] r
  => String
  -> (FilePath -> Sem r b)
  -> Sem r b
safefify p f = do
  exe <- myFindExecutable p
  case exe of
    Nothing -> throw ("Couldn't find \"" <> p <> "\" in PATH")
    Just exe' -> f exe'

safeReadProcess ::
     Members '[ ProcessSemantics, Error String] r
  => String
  -> [String]
  -> String
  -> Sem r String
safeReadProcess p args input =
  safefify p $ \command -> myReadProcess command args input

data MvnResult
  = MvnResultOk
  | MvnResultFailure

parseSafe ::
     Members '[ XmlSemantics, FileSemantics, Error String] r
  => FilePath
  -> Sem r Node
parseSafe f = do
  c <- myReadFile f
  p <- myParseString c
  case p of
    Left e -> throw ("error parsing \"" <> f <> "\": " <> show e)
    Right v -> pure v

nodeContent :: Node -> BS8.ByteString
nodeContent n = foldMap contentToText (contents n)
  where
    contentToText (Text x) = x
    contentToText _ = ""

getVersion ::
     Members '[ XmlSemantics, FileSemantics, Error String] r => Sem r String
getVersion = do
  pom <- parseSafe "pom.xml"
  case listToMaybe (filter ((== "version") . name) (children pom)) of
    Nothing -> throw ("couldn't find \"version\" in pom.xml" :: String)
    Just version -> pure (BS8.unpack (nodeContent version))

rebuildSome ::
     Members '[ Reader Opts, XmlSemantics, LogSemantics, FileSemantics, ProcessSemantics, Resource, Error String, Fail] r
  => [String]
  -> Sem r ExitCode
rebuildSome mods = do
  let modPaths = intercalate "," (("modules/" <>) <$> mods)
  opts <- ask
  result <-
    mvnPretty
      (optsStdout opts)
      (mvnOpts opts <> ["install", "--projects", modPaths])
  whenSuccess result $
    forM_ mods $ \mod -> do
      copyModule (optsTargetAssembly opts) mod
      notifySend $ "rebuild \"" <> unwords mods <> "\" succeeded!"

copyModule ::
     Members '[ XmlSemantics, FileSemantics, LogSemantics, Error String] r
  => Maybe Assembly
  -> FilePath
  -> Sem r ()
copyModule targetAssemblyOpt mod = do
  home <- myGetEnv "HOME"
  version <- getVersion
  let targetAssembly = fromMaybe ("develop-" <> version) targetAssemblyOpt
      ocPath :: FilePath
      ocPath =
        "org" </> "opencastproject" </> ("opencast-" <> mod) </> version </>
        ("opencast-" <> mod <> "-" <> version <> ".jar")
      from :: FilePath
      from = home </> ".m2" </> "repository" </> ocPath
      to :: FilePath
      to =
        "build" </> ("opencast-dist-" <> targetAssembly) </> "system" </> ocPath
  myLog $ "copying " <> from <> " to " <> to
  myCopyFile from to

partialRebuild ::
     Members '[ Reader Opts, XmlSemantics, FileSemantics, Resource, Fail, LogSemantics, Error String, ProcessSemantics, PromptSemantics] r
  => String
  -> Sem r ExitCode
partialRebuild relativeTo = do
  (rebuildType, mods) <- changedModules relativeTo
  fullRebuild' <-
    if rebuildType == FullRebuild
      then myPrompt "\"pom.xml\" changed, do a full rebuild?"
      else pure False
  if fullRebuild'
    then ask >>= fullRebuild
    else do
      opts <- ask
      result <-
        mvnPretty
          (optsStdout opts)
          (mvnOpts opts <>
           ["install", "-pl", intercalate "," (("modules" </>) <$> mods)])
      whenSuccess result $ do
        myLog (show mods)
        forM_ mods (copyModule Nothing)
        notifySend "Partial rebuild complete"

gitDiff ::
     Members '[ ProcessSemantics, Error String] r => String -> Sem r [String]
gitDiff relativeTo =
  lines <$> safeReadProcess "git" ["diff", "--name-only", relativeTo] ""

data RebuildType
  = FullRebuild
  | PartialRebuild
  deriving (Eq)

changedModules ::
     Members '[ ProcessSemantics, Error String] r
  => String
  -> Sem r (RebuildType, [String])
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

whenSuccess :: Monad m => MvnResult -> m () -> m ExitCode
whenSuccess MvnResultOk f = f >> pure ExitSuccess
whenSuccess _ _ = pure (ExitFailure 1)

fullRebuild ::
     Members '[ LogSemantics, FileSemantics, ProcessSemantics, Resource, Fail] r
  => Opts
  -> Sem r ExitCode
fullRebuild opts = do
  result <-
    mvnPretty
      (optsStdout opts)
      (mvnOpts opts <> ["package", "install", "-Pdev"])
  whenSuccess result (notifySend "full rebuild success")

lastLogFileName :: FilePath
lastLogFileName = "last-log.txt"

mvnPretty ::
     Members '[ LogSemantics, FileSemantics, ProcessSemantics, Resource, Fail] r
  => Bool
  -> [String]
  -> Sem r MvnResult
mvnPretty stdout args = do
  myLog ("mvn " <> unwords args)
  (_, Just hout, _, processHandle) <-
    myCreateProcess ((proc "mvn" args) {std_out = CreatePipe})
  errorLines <- myNewIORef []
  bracket (myOpenFile lastLogFileName WriteMode) myHClose $ \lastLog -> do
    errorCode <- mvnWithParse' stdout hout lastLog processHandle errorLines
    case errorCode of
      ExitSuccess -> pure MvnResultOk
      ExitFailure i -> do
        notifySend $ "failure (code " <> show i <> ")!"
        pure MvnResultFailure

mvnWithParse' ::
     Members '[ LogSemantics, FileSemantics, ProcessSemantics] r
  => Bool
  -> Handle
  -> Handle
  -> ProcessHandle
  -> IORef [String]
  -> Sem r ExitCode
mvnWithParse' stdout h lastLog p errLines = do
  eof <- myHIsEOF h
  if eof
    then myWaitForProcess p
    else do
      line <- myHGetLine h
      myHPutStrLn lastLog line
      let lineError :: Bool
          lineError = "[ERROR]" `isPrefixOf` line && line /= "[ERROR] "
          lineUpdate :: Bool
          lineUpdate = "[INFO] Building" `isPrefixOf` line
      when lineError (myLog line >> myModifyIORef errLines (line :))
      when (lineUpdate || (stdout && not lineError)) (myLog line)
      mvnWithParse' stdout h lastLog p errLines

createCa :: Member ProcessSemantics r => String -> Sem r ExitCode
createCa caName = do
  myCallCommand $
    "curl -i -s -f --digest -u opencast_system_account:CHANGE_ME --request POST -H \"X-Requested-Auth: Digest\" --data state=idle 'http://localhost:8080/capture-admin/agents/" <>
    caName <> "'"
  pure ExitSuccess

main' :: IO ExitCode
main' = do
  opts <- execParser (info (optsParser <**> helper) fullDesc)
  if optsShowLastLog opts
    then withFile lastLogFileName ReadMode (hGetContents >=> putStrLn) >>
         pure ExitSuccess
    else case optsCreateCa opts of
           Just caName -> createCa caName & runProcessSemantics & runM
           Nothing -> do
             when
               (isJust (optsRebuildSome opts) && isJust (optsRelativeTo opts))
               (error "can't specify some rebuild and relative rebuild")
             errorOrExitCode <-
               case optsRebuildSome opts of
                 Just x ->
                   rebuildSome x & runXml & runReader opts & runLog &
                   runFileSemantics &
                   runProcessSemantics &
                   failToError id &
                   runError &
                   resourceToIO &
                   runM
                 _ ->
                   case optsRelativeTo opts of
                     Just x ->
                       partialRebuild x & runReader opts & runXml & runLog &
                       runFileSemantics &
                       runProcessSemantics &
                       runPrompt opts &
                       failToError id &
                       runError &
                       resourceToIO &
                       runM
                     _ ->
                       fullRebuild opts & runXml & runLog & runFileSemantics &
                       runProcessSemantics &
                       runPrompt opts &
                       failToError id &
                       runError &
                       resourceToIO &
                       runM
             case errorOrExitCode of
               Left e -> error e
               Right ec -> pure ec

main :: IO ()
main = do
  ec <- main'
  exitWith ec
