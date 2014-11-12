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
particle thePos theVelocity theRadius =
  Particle thePos theVelocity theRadius (theRadius * theRadius)

moveParticle :: Num a => a -> Particle a -> Particle a
moveParticle dt p = p { position = position p +. (dt *. velocity p) }

segEnd :: Num a => LineSegment a -> (a,a)
segEnd seg = segStart seg +. segDir seg

-- Find the square of the distance of a point to line segment
squareDistanceLS :: (Ord a, Fractional a) => (a,a) -> LineSegment a -> a
squareDistanceLS p seg =
  let dirUnit   = segDirUnit seg
      (dx,dy)   = segDir seg
      a         = segStart seg
      q         = p -. a
      r@(rx,ry) = projectUnit dirUnit q
  in if    dx < 0 && 0 >= rx && rx >= dx
        || dx > 0 && 0 <= rx && rx <= dx
        || dy < 0 && 0 >= ry && ry >= dy
        || dy > 0 && 0 <= ry && ry <= dy
     then norm (q -. r) else if rx < 0 then norm (p -. a) else norm (p -. segEnd seg)

particleHitsLS :: (Ord a, Fractional a) => Particle a -> LineSegment a -> Bool
particleHitsLS p seg =
  squareDistanceLS (position p) seg < sqRadius p

collideLS :: (Ord a, Fractional a) => Particle a -> LineSegment a -> Particle a
collideLS p seg | particleHitsLS p seg =
  let v     = velocity p
      vNorm = projectUnit (segNormal  seg) v
      vDir  = projectUnit (segDirUnit seg) v
  in p { velocity = (((-1) *. vNorm) +. vDir) }
                | otherwise = p
