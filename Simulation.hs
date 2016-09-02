module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics

--Converting simulation second to real world second
timeConvert :: Float -> Float -> Float
timeConvert sTime scale = sTime * scale

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle t (Particle m (x,y) (vx,vy)) = (Particle m (x',y') (vx,vy))
	where
		x' = x + vx*t
		y' = y + vy*t

--Return the total acceleration of a particle based on the force of other particles		
acceleration :: Particle -> [Particle] -> Accel
acceleration p1 [] = (0, 0)
acceleration p1 (p2:pr) = (fst(force p1 p2) + fst(acceleration p1 pr), snd(force p1 p2) + snd(acceleration p1 pr))

--Change a particle velocity based on it's acceleration and time
accelerateOne :: Float -> Accel -> Particle -> Particle
accelerateOne t (ax, ay) (Particle m (x,y) (vx,vy)) = (Particle m (x,y) (vx',vy'))
	where
		vx' = vx + ax*t
		vy' = vy + ay*t
		
-- Accelerate a particle in dependence on the gravitational force exerted by all other particles for
-- the given number of (simulated) seconds.
--
accelerate :: Float -> [Particle] -> [Particle]
accelerate _ [] = []
accelerate 0 pl = pl
accelerate t pl = map (\p -> accelerateOne t (acceleration p pl) p) pl

-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ t (World s1 s2 s3 pl) = World s1 s2 s3 (map (\p -> moveParticle (timeConvert t s3) p) (accelerate (timeConvert t s3) pl))