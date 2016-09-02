{-# LANGUAGE ScopedTypeVariables #-}

module World (

  -- constants
  epsilon, width, height, bigG,
  
  -- types
  Mass, Position, Velocity, Accel, Energy, Particle(..), World(..),
  
  -- read a world from a file
  readWorld,

  -- a two-body world
  sunEarthWorld
  
) where
  
import Prelude hiding     (catch)

import Control.Exception  (catch)
import System.Exit        (exitFailure)

  
-- Types & constants
-- -----------------

-- For floating point comparisons
--
epsilon :: Float
epsilon = 0.001

-- Constants
--
width, height :: Int    -- extent of the window; origin is in the center
width  = 600
height = 600

-- Gravitational constant
--
bigG :: Float
bigG = 6.67428e-11                  -- in m^3 kg^(-1) s^(-2)

-- Basic physical measures
--
type Point     = (Float, Float)     -- Same definitions as...
type Vector    = Point              -- in Graphics.Gloss.Picture
type Mass      = Float              -- in kilogram
type Position  = Point              -- in meter
type Velocity  = Vector             -- in meter/second
type Accel     = Vector             -- in meter/second^2
type Energy    = Double             -- in joule

-- We represent particles as mass points at a particular position that have a particular velocity
--
data Particle = Particle Mass Position Velocity
              deriving (Show, Read)
  
-- The world state consists of three scaling factors and a set of particles.  
--
-- * The first scaling factor determines which fraction of a pixel represents one meter.
-- * The second scaling factor determines which fraction of a pixel represents one kilogram when 
--   determining the radius of the circle representing a particle.
-- * The third scaling factor determines how many simulated seconds correspond to one second of real
--   time.
--
data World = World Float Float Float [Particle]
           deriving (Show, Read)


-- Setting up the world
-- --------------------

-- Initialising a two body system modelling the sun and the earth
--
sunEarthWorld :: World
sunEarthWorld = World distanceScale (earthMass / 10000) 1000000
                      [ Particle sunMass (0, 0) (0, 0)
                      , Particle earthMass (distanceSunEarth, 0) (0, earthVelocity)]
  where
    sunMass          = 1.9891e30
    distanceSunEarth = 152098232e3   -- Aphelion
    earthMass        = 5.9736e24
    earthVelocity    = 29.78e3
    --
    distanceScale = (fromIntegral height * 0.4) / distanceSunEarth

-- Read a world model from the given file
--
readWorld :: FilePath -> IO World
readWorld fname
  = do
      contents <- readFile fname
      readIO contents
   `catch` \(exc::IOError) ->
     do 
       putStrLn $ "Fatal error: can't read world description\n" ++ show exc
       exitFailure
    