import System.IO
import System.Environment
import System.Exit
import Data.List

printInps = do input <- getLine
               putStrLn input
               printInps

main = do printInps
