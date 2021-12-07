module Main where

import           Data.Foldable
import           Lamb
import           System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings (withInterrupt loop)
  where
    loop = handleInterrupt loop $ do
      mline <- getInputLine "lamb> "
      for_ mline $ \line -> do
        if null line
          then loop
          else do
            outputStrLn $ either id (show . reduce) $ parseTerm line
            loop
