import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..) )
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D

import Data.Word  (Word8 )
import Data.Fixed ( divMod' )

type R      = Float
type R2     = (R,R)
type Angle  = R

pixelsx:: Int
pixelsx = 1920
pixelsy:: Int
pixelsy = 1200

nbsteps :: Int
nbsteps = 256

centerx :: Float
centerx = -1.8
centery :: Float
centery = 0 


width :: Float
-- width   = (intToFloat pixelsx) * scale
width = 0.1

scale :: Float
scale = width/ (intToFloat pixelsx)

height :: Float
height  = (intToFloat pixelsy) * scale


intToFloat :: Int -> Float
intToFloat = fromInteger . toInteger

newtype Complex = C (Float,Float) deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n,0.0)
    C (x,y) * C (z,t) = C (z*x - y*t, y*z + x*t)
    C (x,y) + C (z,t) = C (x+z, y+t)
    abs (C (x,y))     = C (sqrt (x*x + y*y),0.0)
    signum (C (x,y))  = C (signum x , 0.0)

real :: Complex -> Float
real (C (x,y))    = x

im :: Complex -> Float
im   (C (x,y))    = y

cabs :: Complex -> Float
cabs = real.abs

f :: Complex -> Complex -> Int -> Int
f c z 0 = 0
f c z n = if (cabs z > 2 ) 
             then n
             else f c ((z*z)+c) (n-1)

indexToFloat :: Int -> Int -> Float -> Float -> Float
indexToFloat i pixels center length = 
    mini + (intToFloat i)*m
    where 
        p    = ( intToFloat pixels :: Float )
        mini = center - (length/2.0)
        m    = length / p

mandel :: DIM2 -> R
mandel (Z :. i :. j ) = (intToFloat m :: Float) / (intToFloat nbsteps :: Float)
    where x = indexToFloat j pixelsx centerx width
          y = indexToFloat i pixelsy centery height
          n = f (C (x,y)) (C (0,0)) nbsteps
          m | n == 0 = div (nbsteps * 90) 100
            | otherwise = n

toImage :: Array DIM2 R -> Array DIM3 Word8
toImage arr = R.traverse arr8 (:. 4) chans where
    arr8 = R.map (floor . (*255) . min 1 . max 0) arr
    chans _ (Z :. _ :. _ :. 3) = 255 -- alpha channel
    chans a (Z :. x :. y :. _) = a (Z :. x :. y)

main :: IO ()
main = do
    let arr = R.fromFunction (Z :. pixelsy :. pixelsx) mandel
    D.runIL $ D.writeImage "mandelbrot.png" (toImage arr)
