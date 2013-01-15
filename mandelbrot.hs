module Main where

import System.IO
import System.Environment ( getArgs )

-- Repa
import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..) )
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D
-- import Data.Complex -- Not efficient enough

import Data.Word  (Word8 )
import Data.Fixed ( divMod' )

import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
-- say to not use the Prelude.catch because it is defined in Control.Exception
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (catch)
import Control.Exception

type R      = Double
type R2     = (R,R)
type Angle  = R

-- Structure for global parameters
data Global = Global {
          imgWidth :: Int
        , imgHeight :: Int
        , nbsteps :: Int
        , xpos :: Double
        , ypos :: Double
        , width   :: Double
        , scale   :: Double
        , height  :: Double
        , filename :: String
        } deriving (Show,Read)


intToDouble :: Int -> Double
intToDouble = fromInteger . toInteger

data Complex = C  {-# UNPACK #-} !Double {-# UNPACK #-} !Double  deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C  (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t)  (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)
    abs (C x y)     = C (sqrt (x*x + y*y)) 0.0
    signum (C x y)  = C (signum x) 0.0

real :: Complex -> Double
real (C x y)    = x

im :: Complex -> Double
im   (C x y)    = y

magnitude :: Complex -> Double
magnitude = real.abs

-- f :: Complex Double -> Complex Double -> Int -> Int -- not efficient enough
f c z 0 = 0
f c z n = if (magnitude z > 2 )
             then n
             else f c ((z*z)+c) (n-1)

indexToDouble :: Int -> Int -> Double -> Double -> Double
indexToDouble i pixels center length =
    mini + (intToDouble i)*m
    where
        p    = ( intToDouble pixels :: Double )
        mini = center - (length/2.0)
        m    = length / p

mandel :: Global ->  DIM2-> R
mandel env (Z :. i :. j ) = (intToDouble m :: Double) / (intToDouble (nbsteps env):: Double)
    where x = indexToDouble j (imgWidth env) (xpos env) (width env)
          y = indexToDouble i (imgHeight env) (ypos env) (height env)
          n = f (C x y) (C 0 0) (nbsteps env)
          m | n == 0 = div ((nbsteps env) * 90) 100
            | otherwise = n


toImage :: Array R.D DIM2 R -> D.Image
toImage arr = D.RGBA $
                force $
                  R.traverse arr8 (:. 4) chans
              where
                arr8 = R.map (floor . (*255) . min 1 . max 0) arr
                chans a (Z :. x :. y :. 0) =   redFromDouble $ a (Z :. x :. y)
                chans a (Z :. x :. y :. 1) = greenFromDouble $ a (Z :. x :. y)
                chans a (Z :. x :. y :. 2) =  blueFromDouble $ a (Z :. x :. y)
                chans _ (Z :. _ :. _ :. 3) = 255
                t n = round $ 255 * ( 0.5 + 0.5*cos( fromIntegral n / 10 ) )
                redFromDouble = t
                greenFromDouble x = t (x+5)
                blueFromDouble x = t (x+10)

force = runIdentity . R.computeP

initGlobalParams :: [String] -> Global
initGlobalParams args = Global {
              imgWidth = read (args !! 0)
            , imgHeight = read (args !! 1)
            , nbsteps = read (args !! 2)
            , xpos = read (args !! 3)
            , ypos = read (args !! 4)
            , width   = read (args !! 5)
            , scale   = sc
            , height  = (read (args !! 1)) * sc
            , filename = args !! 6 }
        where
            sc = (read (args !! 5)) / (read $ head args)

matrix width height = Z :. height :. width

printUsage :: IO ()
printUsage = hPutStrLn stderr "mandelbrot w h nb x y realwidth scale filename"

main :: IO ()
main = do
    args <- getArgs
    if length args < 6
      then printUsage
      else do
        env <- return $ initGlobalParams args
        print $ env
        surface <- return $  matrix (imgWidth env) (imgHeight env)
        arr <- return $ R.fromFunction surface (mandel env)
        removeIfExists (filename env)
        D.runIL $ D.writeImage (filename env) (toImage arr)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

