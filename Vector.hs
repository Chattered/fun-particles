module Vector (UnitV, fromUnitVector, normalise,
               (+.), (-.), (*.), dot, norm, magnitude, project, projectUnit) where

newtype UnitV a = UnitV (a,a)

fromUnitVector :: UnitV a -> (a,a)
fromUnitVector (UnitV v) = v

(-.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(ux,uy) -. (vx,vy) = (ux-vx,uy-vy)

(+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(ux,uy) +. (vx,vy) = (ux+vx,uy+vy)

(*.) :: Num a => a -> (a, a) -> (a, a)
k *. (x,y) = (k*x,k*y)

(/.) :: Fractional a => (a, a) -> a -> (a, a)
(x,y) /. k = (x/k, y/k)

dot :: Num a => (a, a) -> (a, a) -> a
dot (ux,uy) (vx,vy) = ux*vx+uy*vy

norm :: Num a => (a,a) -> a
norm v = v `dot` v

magnitude :: Floating a => (a,a) -> a
magnitude v = sqrt (norm v)

project :: Floating a => (a,a) -> (a,a) -> (a,a)
project u v = ((u `dot` v) / magnitude u) *. u

projectUnit :: Num a => UnitV a -> (a,a) -> (a,a)
projectUnit (UnitV u) v = (u `dot` v) *. u

normalise :: Floating a => (a,a) -> UnitV a
normalise v = UnitV (v /. magnitude v)
