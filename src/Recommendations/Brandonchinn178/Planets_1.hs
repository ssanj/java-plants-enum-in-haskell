module Recommendations.Brandonchinn178.Planets_1(runPlanets) where

import Data.Foldable (traverse_)

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


surfaceGravity :: Planet -> Double
surfaceGravity planet =
    let (Mass m)   = mass planet
        (Radius r) = radius planet
    in gConstant * m / (r * m)


surfaceWeight :: Mass -> Planet -> Double
surfaceWeight (Mass otherMass) planet =
    let sg = surfaceGravity planet
    in otherMass * sg


runPlanets :: Double -> IO ()
runPlanets sampleWeight =
    let earthSurfaceGravity = surfaceGravity EARTH

        massOnEarth :: Mass
        massOnEarth = Mass $ sampleWeight / earthSurfaceGravity

        planetToWeight = map (\p -> (p, surfaceWeight massOnEarth p)) [minBound .. maxBound]
        render (p, weight) = "Your weight on " <> show p <> " is " <> (show weight)

    in mapM_ (putStrLn . render) planetToWeight

