{-# LANGUAGE OverloadedStrings #-}
module WeatherParse where

import Control.Monad (mzero)

import Data.Time.Clock
import Data.Time.Format
import Data.Maybe (fromMaybe)

import Data.Aeson

-- {{{ a city, as we get it from openweathermap
data City = City
    { name :: String
    , country :: String
    } deriving (Show, Read, Eq)

instance FromJSON City where
    parseJSON (Object v) =
        City <$> v .: "name" <*> v .: "country"
    parseJSON _ = mzero
-- }}}

-- {{{ Newtype wrapper for our description, needed due to our nested document structure
newtype Description = Description String
    deriving (Show, Read, Eq)

instance FromJSON Description where
    parseJSON (Object v) = Description <$> (v .: "description")
    parseJSON _ = mzero
-- }}}

-- {{{ Newtype wrapper for rain/snow, needed due to bad formatting.
newtype Precipitation = Precipitation (Maybe Double)
    deriving (Show, Read, Eq, Ord)

instance FromJSON Precipitation where
    parseJSON (Object v) = Precipitation <$> (v .:? "3h")
    parseJSON _ = mzero
-- }}}

-- {{{ Weather at some point in time
data Weather = Weather
    { time :: Maybe UTCTime
    , temp :: Double
    , tempMin :: Double
    , tempMax :: Double
    , pressure :: Double
    , humidity :: Int
    , description :: [Description]
    , clouds :: Int
    , windSpeed :: Double
    , windDir :: Double
    , rain :: Precipitation
    , snow :: Precipitation
    } deriving (Show, Read, Eq)

instance FromJSON Weather where
    parseJSON (Object v) = Weather <$>
        fmap parseDt (v .: "dt_txt") <*>
        ((v .: "main") >>= (.: "temp")) <*>
        ((v .: "main") >>= (.: "temp_min")) <*>
        ((v .: "main") >>= (.: "temp_max")) <*>
        ((v .: "main") >>= (.: "pressure")) <*>
        ((v .: "main") >>= (.: "humidity")) <*>
        ((v .: "weather") >>= parseJSON) <*>
        ((v .: "clouds") >>= (.: "all")) <*>
        ((v .: "wind") >>= (.: "speed")) <*>
        ((v .: "wind") >>= (.: "deg")) <*>
        -- explanation of the two lines below:
        -- there may, or may not be objects with the name "rain"/"snow".
        -- if they *are* present, they might be empty.
        -- thus, we get a Parser (Maybe Value) via .:? and
        -- pass it to a fromMaybe lifted into the Parser monad.
        -- this way we get a Parser Value either way, effectively
        -- simulating an empty object to the FromJSON instance of
        -- Precipitation, which uses .:?, too. hacky, but reliable,
        -- considering the shitty input we have.
        (fmap (fromMaybe (object [])) (v .:? "rain") >>= parseJSON) <*>
        (fmap (fromMaybe (object [])) (v .:? "snow") >>= parseJSON)
    parseJSON _ = mzero
-- }}}

-- Parse time from our data
parseDt :: String -> Maybe UTCTime
parseDt = parseTimeM True defaultTimeLocale "%F %T"

-- {{{ The whole forecast for a city
data Forecast = Forecast
    { city :: City
    , forecastData :: [Weather]
    } deriving (Show, Read, Eq)

instance FromJSON Forecast where
    parseJSON (Object v) =
        Forecast <$> (v .: "city") <*> (parseJSON =<< (v .: "list"))
    parseJSON _ = mzero
-- }}}
