import System.IO
import System.Process
import System.Exit
import System.Directory
import Control.Exception

-- This function prompts for user input
-- while displaying a string
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Defines a try that will catch the proper exceptions
try' :: IO a ->  IO (Either IOException a)
try' =  try 

-- Processes the line given by the user and executes the
-- appropriate command
process :: [String] -> IO ()
process list = do
  let (cmd:args) = list

  case cmd of
    "exit" -> exitSuccess
    "cd" -> setCurrentDirectory (args !! 0)
    _ -> do
      s <- try' $ createProcess (proc cmd args)
      case s of
        Left ex -> print ex
        Right (_, _, _, ph) -> do _ <- waitForProcess ph
                                  return ()

  return ()

main::IO()
main = do
  line <- prompt "$ "
  process (words line)
  main

