module Visualise (visualiseWorld) where

import World
import Graphics.Gloss

-- Visualise the world
--
visualiseWorld :: World -> Picture
visualiseWorld (World sf unitMass _ ps) = Scale sf sf $ Pictures (map visualiseParticle ps)
  where
    visualiseParticle (Particle m (x, y) _) = Translate x y $ Color white $ solidCircle size
      where
        size = logBase 10 (m / unitMass) / sf

solidCircle :: Float -> Picture
solidCircle r = ThickCircle (r/2) r