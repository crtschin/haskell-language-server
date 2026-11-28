module Main where

import           SharedApi (sharedValue)

main :: IO ()
main = print sharedValue
