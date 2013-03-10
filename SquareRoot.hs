module SquareRoot where

import Data.Ratio

sqrt' :: Rational -> Rational
sqrt' x = rSqrt x (1%1000)

-- The square root of x, within error tolerance e
-- Warning: diverges if e=0 and x is not a perfect square
rSqrt :: Rational -> Rational -> Rational
rSqrt x _ | x < 0 = error "rSqrt of negative"
rSqrt _ e | e < 0 = error "rSqrt negative tolerance"
rSqrt x e = firstWithinE $ map (uncurry (%)) $ convergents $
            sqrtCF (numerator x) (denominator x)
  where
    firstWithinE = head . dropWhile off
    off y = abs (x - y^2 - e^2) > 2 * e * y

-- The continued fraction for the square root of positive p%q
sqrtCF :: Integer -> Integer -> [Integer]
sqrtCF p q = cf 1 0 1
  where
    -- Continued fraction for (a*sqrt(p%q)+b)/c
    cf a b c
     | c == 0    = []
     | otherwise = x : cf (a' `div` g) (b' `div` g) (c' `div` g)
     where
       x  = (iSqrt (p*a*a) q + b) `div` c
       a' = a * c * q
       e  = c * x - b
       b' = c * e * q
       c' = p*a*a - q*e*e
       g  = foldl1 gcd [a', b', c']

-- The greatest integer less than or equal to sqrt (p%q) for positive p%q
iSqrt :: Integer -> Integer -> Integer
iSqrt p q = sq 0 (1 + p `div` q)
  where
    sq y z
     | z - y < 2      = y
     | q * m * m <= p = sq m z
     | otherwise      = sq y m
     where m = (y + z) `div` 2

-- The following could go in a separate module
-- for general continued fractions

-- The convergents of a continued fraction
convergents :: [Integer] -> [(Integer, Integer)]
convergents cf = drop 2 $ zip h k
  where
    h = 0 : 1 : zipWith3 affine cf (drop 1 h) h
    k = 1 : 0 : zipWith3 affine cf (drop 1 k) k
    affine x y z = x * y + z

