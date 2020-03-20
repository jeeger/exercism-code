module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =  seconds / (planetFactor planet * 31557600)
  where planetFactor Mercury = 0.2408467
        planetFactor Venus = 0.61519726
        planetFactor Mars = 1.8808158
        planetFactor Jupiter = 11.862615
        planetFactor Saturn = 29.447498
        planetFactor Uranus = 84.016846
        planetFactor Neptune = 164.79132
        planetFactor Earth = 1
