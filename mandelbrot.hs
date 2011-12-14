import System.Environment ( getArgs )
import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..) )
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D
-- import Data.Complex -- Not efficient enough

import Data.Word  (Word8 )
import Data.Fixed ( divMod' )

import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)
-- say to not use the Prelude.catch because it is defined in Control.Exception
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (catch) 
import Control.Exception

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

type R      = Float
type R2     = (R,R)
type Angle  = R

-- Structure for global parameters
data Global = Global { 
          pixelsx :: Int
        , pixelsy :: Int
        , nbsteps :: Int
        , centerx :: Float
        , centery :: Float
        , width   :: Float
        , scale   :: Float
        , height  :: Float 
        , filename :: String 
        } deriving (Show,Read)


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

magnitude :: Complex -> Float
magnitude = real.abs

-- f :: Complex Float -> Complex Float -> Int -> Int -- not efficient enough
f c z 0 = 0
f c z n = if (magnitude z > 2 ) 
             then n
             else f c ((z*z)+c) (n-1)

indexToFloat :: Int -> Int -> Float -> Float -> Float
indexToFloat i pixels center length = 
    mini + (intToFloat i)*m
    where 
        p    = ( intToFloat pixels :: Float )
        mini = center - (length/2.0)
        m    = length / p

mandel :: Global ->  DIM2-> R
mandel env (Z :. i :. j ) = (intToFloat m :: Float) / (intToFloat (nbsteps env):: Float)
    where x = indexToFloat j (pixelsx env) (centerx env) (width env)
          y = indexToFloat i (pixelsy env) (centery env) (height env)
          n = f (C(x,y)) (C(0,0)) (nbsteps env)
          m | n == 0 = div ((nbsteps env) * 90) 100
            | otherwise = n

toImage :: Array DIM2 R -> Array DIM3 Word8
toImage arr = R.traverse arr8 (:. 4) chans where
    arr8 = R.map (floor . (*255) . min 1 . max 0 . sqrt ) arr
    chans _ (Z :. _ :. _ :. 3) = 255 -- alpha channel
    chans a (Z :. x :. y :. _) = a (Z :. x :. y)

main :: IO ()
main = do
    args <- getArgs
    let
        sc = (read (args !! 5)) / (read $ head args) 
        params = Global {
              pixelsx = read (args !! 0)
            , pixelsy = read (args !! 1)
            , nbsteps = read (args !! 2)
            , centerx = read (args !! 3)
            , centery = read (args !! 4)
            , width   = read (args !! 5)
            , scale   = sc
            , height  = (read (args !! 1)) * sc
            , filename = args !! 6 }
        arr = R.fromFunction (Z :. (pixelsy params) :. (pixelsx params)) (mandel params)
    removeIfExists (filename params)
    D.runIL $ D.writeImage (filename params) (toImage arr)
