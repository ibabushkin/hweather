module WeatherFetch where

import Data.Maybe (fromJust)

import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy as L

-- {{{ Units for temperature + speed
data Unit
    = Default
    | Metric
    | Imperial
    deriving (Show, Read, Eq)

-- generate a string that can be fit into an URL
showUnitUrl :: Unit -> String
showUnitUrl Default  = ""
showUnitUrl Metric   = "&units=metric"
showUnitUrl Imperial = "&units=imperial"
-- }}}

-- {{{ Locations come in various flavours
data Location
    = City
        { name :: String
        , countryCode :: String
        }
    | CityID
        { id :: String }
    | Coords
        { latitude :: Double
        , longitude :: Double
        }
    deriving (Show, Read, Eq)

-- {{{ Boilerplate for our arg handling
setCity :: Location -> String -> Location
setCity (City _ c) s = City s c
setCity _ s = City s ""

setCountry :: Location -> String -> Location
setCountry (City c _) s = City c s
setCountry _ s = City "" s

setLat :: Location -> Double -> Location
setLat (Coords _ l) d = Coords d l
setLat _ d = Coords d 0

setLon :: Location -> Double -> Location
setLon (Coords l _) d = Coords l d
setLon _ d = Coords 0 d
-- }}}

-- generate a string that can be fit into an URL
showLocationURL :: Location -> String
showLocationURL (City n cC) = "q=" ++ n ++ "," ++ cC
showLocationURL (CityID i) = "id=" ++ i
showLocationURL (Coords lat lon) = "lat=" ++ show lat ++ "&lon=" ++ show lon
-- }}}

-- build an URL that represents the user's needs
constructURL :: String -> Unit -> Location -> String
constructURL appId unit location = escapeURIString (/=' ') $
    "http://api.openweathermap.org/data/2.5/forecast?" ++
        showLocationURL location ++ showUnitUrl unit ++ "&APPID=" ++ appId

-- fetch a 5 day (every 3 hours) forecast
getRawWeatherdata :: String -> Unit -> Location -> IO L.ByteString
getRawWeatherdata a u l = simpleHTTP req >>= getResponseBody
    where uri = fromJust $ parseURI (constructURL a u l)
          req = Request uri GET [] L.empty
