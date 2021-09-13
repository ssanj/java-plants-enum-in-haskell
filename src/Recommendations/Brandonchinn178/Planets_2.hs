module Recommendations.Brandonchinn178.Planets_2(runPlanets) where

import Data.Foldable (forM_, traverse_)

-- Haskell implementation of the Java Enum: Planets example
-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

import Data.Foldable (traverse_)


data Planet = MERCURY
            | VENUS
            | EARTH
            | MARS
            | JUPITER
            | SATURN
            | URANUS
            | NEPTUNE deriving (Enum, Bounded, Show)


newtype Mass = Mass Double


newtype Radius = Radius Double


newtype SurfaceGravity = SurfaceGravity Double


newtype SurfaceWeight = SurfaceWeight Double deriving Show

gConstant :: Double
gConstant = 6.67300E-11


radius :: Planet -> Radius
radius MERCURY = Radius 2.4397e6
radius VENUS   = Radius 6.0518e6
radius EARTH   = Radius 6.37814e6
radius MARS    = Radius 3.3972e6
radius JUPITER = Radius 7.1492e7
radius SATURN  = Radius 6.0268e7
radius URANUS  = Radius 2.5559e7
radius NEPTUNE = Radius 2.4746e7


mass :: Planet -> Mass
mass MERCURY = Mass 3.303e+23
mass VENUS   = Mass 4.869e+24
mass EARTH   = Mass 5.976e+24
mass MARS    = Mass 6.421e+23
mass JUPITER = Mass 1.9e+27
mass SATURN  = Mass 5.688e+26
mass URANUS  = Mass 8.686e+25
mass NEPTUNE = Mass 1.024e+26


surfaceGravity :: Planet -> SurfaceGravity
surfaceGravity planet =
    let (Mass m)   = mass planet
        (Radius r) = radius planet
    in SurfaceGravity $ gConstant * m / (r * m)


surfaceWeight :: Mass -> Planet -> SurfaceWeight
surfaceWeight (Mass otherMass) planet =
    let (SurfaceGravity sg)= surfaceGravity planet
    in SurfaceWeight $ otherMass * sg


runPlanets :: Double -> IO ()
runPlanets earthWeight =
  forM_ [minBound .. maxBound] $ \planet -> do
    let newWeight = surfaceWeight mass planet
    putStrLn $ "Your weight on " <> show planet <> " is " <> (show newWeight)
  where
    mass =
      let SurfaceGravity earthGravity = surfaceGravity EARTH
      in Mass $ earthWeight / earthGravity
