module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Color as Col
import qualified Graphics.Gloss.Data.Picture as Pic
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Particle

initParticles :: [Particle Float]
initParticles = [particle (50,150) (500,0) 10]

initBarriers :: [LineSegment Float]
initBarriers = [lineSegment (-200,-200) (0,50)
               ,lineSegment (0,50) (200,-150)
               ,lineSegment (200,-150) (50,250)
               ,lineSegment (50,250) (-200,-200)]

renderParticle :: Particle Float -> Picture
renderParticle p = Pic.Translate x y (Pic.Color Col.white (Pic.Circle (radius p)))
  where (x,y) = position p

renderBarrier :: LineSegment Float -> Picture
renderBarrier seg =
  Pic.Color Col.white (Pic.Line [segStart seg, segEnd seg])

render :: ([Particle Float], [LineSegment Float]) -> Picture
render (particles,barriers) =
  Pictures (map renderParticle particles ++ map renderBarrier barriers)

step :: (Ord a, Fractional a) => ViewPort -> a -> ([Particle a], [LineSegment a])
     -> ([Particle a], [LineSegment a])
step _ dt (particles, barriers) =
  ([ foldl collideLS p2 barriers | p <- particles, let p2 = moveParticle dt p ],
   barriers)
  
main :: IO ()
main = simulate (FullScreen (1280,800)) (Col.makeColor 0 0 0 0) 60
       (initParticles, initBarriers)
       render
       step
