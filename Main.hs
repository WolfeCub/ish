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

-- Executes cmd with arguments args with error handling
executeCommand :: String -> [String] -> IO ()
executeCommand cmd args = do
    s <- try' $ createProcess (proc cmd args)
    case s of
        Left ex -> print ex
        Right (_, _, _, ph) -> do _ <- waitForProcess ph
                                  return ()

-- Processes the line given by the user 
process :: [String] -> IO ()
process line = do

  case line of
    [] -> return ()
    _ -> let (cmd:args) = line in

        case cmd of
            "exit" -> exitSuccess
            "cd" -> setCurrentDirectory (args !! 0)
            _ -> executeCommand cmd args

main::IO()
main = do
  line <- prompt "$ "
  process (words line)
  main

