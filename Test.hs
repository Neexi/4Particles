module Test where

import Test.QuickCheck
import World
import TestSupport
import Physics
import Simulation

prop_EnergyConservation :: World -> Bool
prop_EnergyConservation (World _ _ _ []) = True
prop_EnergyConservation w =
	abs(worldEnergy (advanceWorld 0 0.001 w) - worldEnergy w)/(worldEnergy w) < realToFrac epsilon