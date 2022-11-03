module Main where

import Instant.Abs (Program)
import Instant.Lex (tokens)
import Instant.Par (myLexer, pProgram)
import Instant.ErrM
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import System.Directory.Internal.Prelude (exitFailure, getArgs, hPutStrLn)
import System.Exit (exitSuccess, ExitCode (ExitSuccess, ExitFailure), exitWith)
import System.FilePath (takeBaseName, replaceExtension, takeDirectory)
import System.IO (stderr)
import JvmCompiler (compile)
import System.Process (readProcessWithExitCode)

tokenize :: String -> ExceptT String IO Program
tokenize s = case pProgram $ myLexer s of
  Bad str -> throwE str
  Ok prog -> return prog
  _ -> error "???"

runCompile :: String -> String -> ExceptT String IO String
runCompile s className = do
  tokens <- tokenize s
  compile tokens className

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      let className = takeBaseName file
      let jasmin = replaceExtension file "j"
      let outPath = takeDirectory file
      program <- readFile file
      result <- runExceptT $ runCompile program className
      case result of
        Left err -> do
          hPutStrLn stderr err
          exitFailure 
        Right compiled -> do
        writeFile jasmin compiled
        (exitcode, _, err) <- readProcessWithExitCode "java" ["-jar", "./lib/jasmin.jar", "-d", outPath, jasmin] ""
        case exitcode of
          ExitSuccess ->
              exitSuccess
          ExitFailure i -> do
              hPutStrLn stderr err
              exitFailure
    _ -> exitFailure