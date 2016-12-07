import System.Process
import System.Exit
import System.Directory
import Control.Exception
import System.Console.Haskeline
import Control.Monad.IO.Class

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

  let (cmd:args) = line in
      case cmd of
          "exit" -> exitSuccess
          "cd" -> setCurrentDirectory (args !! 0)
          _ -> executeCommand cmd args

main :: IO()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "$ "
      case minput of
        Nothing -> return ()
        Just input -> liftIO (process $ words input)
      loop            
