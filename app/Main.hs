module Main (main) where

import Sets
import Boards

main :: IO ()
main = do 
    renderAllBoards
    putStrLn "done with svgs"
    writeAllSets
