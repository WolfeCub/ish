import System.Process
import System.Exit
import System.Directory
import Control.Exception
import System.Console.Haskeline
import Control.Monad.IO.Class
import System.FilePath.Posix

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

-- Changes directory as specified by args
-- If no args goes to home directory
changeDirectory :: [FilePath] -> IO ()
changeDirectory args = do case args of
                            [] -> do
                              home <- getHomeDirectory
                              setCurrentDirectory home
                              return ()
                            _ -> setCurrentDirectory $ head $ args

checkAlias :: [String] -> [String]
checkAlias line
  | head line == "ls" = line ++ ["--color=always"]
  | head line == "grep" = line ++ ["--color=always"]
  | otherwise = line

-- Processes the line given by the user 
process :: [String] -> IO ()
process line = do
  let (cmd:args) = checkAlias line in
      case cmd of
          "exit" -> exitSuccess
          "cd" -> changeDirectory args
          _ -> executeCommand cmd args

main :: IO()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      homeDir <- liftIO getHomeDirectory
      workingDir <- liftIO getCurrentDirectory
      let relativeWD = makeRelative homeDir workingDir
  
      let relativePath
            | relativeWD == "." = "~"
            | isAbsolute relativeWD = relativeWD
            | otherwise = "~/" ++ relativeWD
       
      minput <- getInputLine $ "\ESC[34m\STX" ++ relativePath ++ " $\ESC[37m\STX "
      case minput of
        Nothing -> return ()
        Just input -> liftIO (process $ words input)
      loop            
