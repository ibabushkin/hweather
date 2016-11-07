module Format where

import System.Console.ANSI

import Data.List
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as F

import Text.Printf (printf)

import WeatherParse
import WeatherFetch

-- how do we want to display something?
data OutputFormat
    = Plaintext
    | Pango
    | ANSI
    deriving (Show, Read, Eq)

-- format a forecast object
formatForecast :: OutputFormat -> Unit -> Forecast -> String
formatForecast outFormat unit (Forecast c fD) =
    formatCity outFormat c ++ concatMap (formatWeather outFormat unit) fD

-- format a city object
formatCity :: OutputFormat -> WeatherParse.City -> String 
formatCity outFormat (WeatherParse.City n c) =
    format outFormat "Weather for " ++ n ++ ", " ++ c ++ "\n"
    
-- format a weather object
formatWeather :: OutputFormat -> Unit -> Weather -> String
formatWeather outFormat u w = intercalate "\n" ls
    where ls = [ fF (formatTime (time w) ++ " - " ++ descs ++ " ") ++
                   fF ("[" ++ show (clouds w) ++ "% clouds]")
               , fF (fTU (temp w) ++ " (" ++ fTU (tempMin w) ++
                   "/" ++ fTU (tempMax w) ++ ")")
               , fF (show (pressure w) ++ "hPa, " ++ show (humidity w) ++
                   "% humidity" ++ rain'' ++ snow'')
               , fF ("Wind from " ++ show (windDir w) ++ "°, at " ++
                   fSU (windSpeed w)) 
               , "\n"
               ] 
          descs = unwords $ map (\(Description s) -> s) (description w)
          fF = format outFormat 
          fTU = formatTemp u
          fSU = formatSpeed u
          rain' = formatMm (rain w)
          rain''
              | rain' /= "" = rain' ++ " rain"
              | otherwise = ""
          snow' = formatMm (snow w)
          snow''
              | snow' /= "" = snow' ++ " snow"
              | otherwise = ""

-- format a temperature according to unit system 
formatTemp :: Unit -> Double -> String
formatTemp u v = show v ++ f u
    where f Metric = "°C"
          f Imperial = "°F"
          f Default = "°K"

-- format a windspeed according to unit system 
formatSpeed :: Unit -> Double -> String
formatSpeed u v = show v ++ f u
    where f Imperial = "mph"
          f _ = "m/s"

-- format a rain / snow value, in millimeters
formatMm :: Precipitation -> String
formatMm (Precipitation (Just d)) = printf ", %.2f" d ++ "mm"
formatMm _ = ""

-- format a timestamp as saved in a Weather object
formatTime :: Maybe UTCTime -> String
formatTime = maybe "" (F.formatTime F.defaultTimeLocale "%A, %d.%m.%Y %H:%M")

-- format a string according to args:
format :: OutputFormat -> String -> String
format Plaintext s = s
format Pango s
    | head s == '[' = yellow
    | "hPa" `isInfixOf` s = red
    | otherwise = lightBlue
    where cS = "</span>"
          lightBlue = "<span color=\"lightblue\">" ++ s ++ "</span>"
          red = "<span color=\"red\">" ++ s ++ cS
          yellow = "<span color=\"yellow\">" ++ s ++ cS
format ANSI s
    | head s == '[' = yellow
    | "hPa" `isInfixOf` s = red
    | otherwise = lightBlue
    where cS = setSGRCode []
          lightBlue = setSGRCode
              [SetColor Foreground Vivid Cyan] ++ s ++ cS
          red = setSGRCode
              [SetColor Foreground Vivid Red] ++ s ++ cS
          yellow = setSGRCode
              [SetColor Foreground Vivid Yellow] ++ s ++ cS
