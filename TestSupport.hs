module TestSupport where

import Control.Monad   (liftM, liftM3)
import Test.QuickCheck

import World


instance Arbitrary Particle where
  arbitrary = oneof [liftM3 (\f -> Particle (baseWeight * abs f)) 
                            (resize 1 arbitrary) 
                            arbitrary 
                            (resize 10 arbitrary)]
    where
--      baseWeight = 5e14  -- good for animation, BAD for QuickCheck
      baseWeight = 5e10  -- good for QuickCheck
    
instance Arbitrary World where
  arbitrary = oneof [liftM (World 1 9.42590890872e11 1) arbitrary]