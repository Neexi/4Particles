module Main where

import Control.Monad      (when)
import System.Environment (getArgs)  
import System.Exit        (exitFailure)
import Graphics.Gloss

import World
import Visualise
import Physics
import Simulation


-- Launch the simulation
--
main :: IO ()
main 
  = do
      args <- getArgs
      when (length args /= 1) $ do
        putStrLn "Usage: particles FILE"
        putStrLn "  where FILE contains a world description"
        exitFailure
      initialWorld <- readWorld (head args)
      simulate 
        (InWindow 
          "Particles"      -- window name
          (width, height)  -- window size
          (10, 10)         -- initial window position
        )
        black            -- background colour
        30               -- number of simulation steps per second
        initialWorld     -- initial state of the world
        visualiseWorld   -- convert a world state into a picture
        advanceWorld     -- progress the world state by a specific time interval