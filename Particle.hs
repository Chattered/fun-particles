module Particle (LineSegment, lineSegment, segStart, segEnd
                ,Particle, particle, position, radius, moveParticle, collideLS) where

import Vector

data LineSegment a = LineSegment { segStart   :: (a,a)
                                 , segDir     :: (a,a)
                                 , segDirUnit :: UnitV a
                                 , segNormal  :: UnitV a
                                 }

lineSegment :: Floating a => (a, a) -> (a, a) -> LineSegment a
lineSegment start end = LineSegment start (end -. start)
                        (normalise $ (end -. start))
                        (normalise $ (-dirY, dirX))
  where (dirX,dirY) = end -. start

data Particle a = Particle { position :: (a,a)
                           , velocity :: (a,a)
                           , radius   :: a
                           , sqRadius :: a
                           }

particle :: Num a => (a,a) -> (a,a) -> a -> Particle a
particle pos velocity radius = Particle pos velocity radius (radius * radius)


moveParticle :: Num a => a -> Particle a -> Particle a
moveParticle dt (Particle pos velocity radius sqRadius) =
  Particle (pos +. (dt *. velocity)) velocity radius sqRadius

segEnd :: Num a => LineSegment a -> (a,a)
segEnd seg = segStart seg +. segDir seg

-- Find the point on a line-segment ab closest to a given point
nearestPointLS :: (Ord a, Fractional a) => (a,a) -> LineSegment a -> (a,a)
nearestPointLS p@(x,y) seg =
  let dir              = segDir seg
      a                = segStart seg
      b                = segEnd seg
      numerator        = (p -. a) `dot` dir
      denominator      = norm dir
      u                = numerator / denominator
  in if 0 <= u && u <= 1 then (u *. dir) +. a else if u < 0 then a else b

particleHitsLS :: (Ord a, Fractional a) => Particle a -> LineSegment a -> Bool
particleHitsLS p seg =
  norm (nearestPointLS (position p) seg -. position p) < sqRadius p

collideLS :: (Ord a, Fractional a) => Particle a -> LineSegment a -> Particle a
collideLS p@(Particle pos velocity radius sqRadius) seg | particleHitsLS p seg =
  let u      = projectUnit (segNormal  seg) velocity
      v      = projectUnit (segDirUnit seg) velocity
      lsDir  = fromUnitVector (segDirUnit seg)
      lsNorm = fromUnitVector (segNormal  seg)
  in Particle pos (((-u) *. lsNorm) +. (v *. lsDir)) radius sqRadius
                                                        | otherwise = p
