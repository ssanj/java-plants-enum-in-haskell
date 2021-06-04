module Planets(runPlanets) where

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


data PlanetStat =
    PlanetStat {
        mass   :: Mass
    ,   radius :: Radius
    }


newtype SurfaceGravity = SurfaceGravity Double


newtype SurfaceWeight = SurfaceWeight Double

gConstant :: Double
gConstant = 6.67300E-11

planetStat :: Planet -> PlanetStat
planetStat MERCURY = PlanetStat (Mass 3.303e+23) (Radius 2.4397e6 )
planetStat VENUS   = PlanetStat (Mass 4.869e+24) (Radius 6.0518e6 )
planetStat EARTH   = PlanetStat (Mass 5.976e+24) (Radius 6.37814e6)
planetStat MARS    = PlanetStat (Mass 6.421e+23) (Radius 3.3972e6 )
planetStat JUPITER = PlanetStat (Mass 1.9e+27  ) (Radius 7.1492e7 )
planetStat SATURN  = PlanetStat (Mass 5.688e+26) (Radius 6.0268e7 )
planetStat URANUS  = PlanetStat (Mass 8.686e+25) (Radius 2.5559e7 )
planetStat NEPTUNE = PlanetStat (Mass 1.024e+26) (Radius 2.4746e7 )


surfaceGravity :: Planet -> SurfaceGravity
surfaceGravity planet =
    let (PlanetStat (Mass mass) (Radius radius)) = planetStat planet
    in SurfaceGravity $ gConstant * mass / (radius * radius)


surfaceWeight :: Mass -> Planet -> SurfaceWeight
surfaceWeight (Mass otherMass) planet =
    let (SurfaceGravity sg)= surfaceGravity planet
    in SurfaceWeight $ otherMass * sg


runPlanets :: Double -> IO ()
runPlanets earthWeight =
    let (SurfaceGravity earthSurfaceGravity) = surfaceGravity EARTH

        massOnEarth :: Mass
        massOnEarth = Mass $ earthWeight / earthSurfaceGravity

        planetValues :: [Planet]
        planetValues = [(minBound :: Planet) .. (maxBound :: Planet)]

        printSurfaceWeight :: Planet -> SurfaceWeight -> String
        printSurfaceWeight planet (SurfaceWeight sw) = "Your weight on " <> (show planet) <> " is " <> (show sw)

        planetStatsStrings :: [String]
        planetStatsStrings = (\p -> printSurfaceWeight p (surfaceWeight massOnEarth p)) <$> planetValues
    in
       traverse_ putStrLn planetStatsStrings
