import System.IO
import System.Exit
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Text.Read (readMaybe)

import Data.Maybe (fromMaybe, isJust)

import Data.Aeson (decode)

import qualified WeatherFetch as Fetch
import WeatherParse
import Format

readWrapper :: (Read a) => String -> a
readWrapper s = fromMaybe
    (error "You passed a wrong value, check your args!") (readMaybe s)

-- not many right now
data Options = Options { optUnit :: !Fetch.Unit
                       , optNumDays :: Int
                       , optAPIKey :: String
                       , optLocation :: !Fetch.Location
                       , optOutput :: String -> IO ()
                       , optFormat :: !OutputFormat
                       }

defaultOptions :: Options
defaultOptions = Options { optUnit = Fetch.Default
                         , optNumDays = 1
                         , optAPIKey = ""
                         , optLocation = Fetch.City "Berlin" "de"
                         , optOutput = putStrLn
                         , optFormat = ANSI
                         }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "u" ["unit"]
        (ReqArg
            (\arg opt -> return opt { optUnit = readWrapper arg })
            "UNIT")
        "Units to use. Possible values are:\n\
        \'Default' (°K, m/s), 'Metric' (°C, m/s)\n\
        \and 'Imperial' (°F, mph)"

    , Option "n" ["numdays"]
        (ReqArg
            (\arg opt -> return opt { optNumDays = let n = readWrapper arg
                                                    in if n > 5 then 5 else n })
            "NUMBER")
        "Number of days to display the forecast for.\n\
        \Should be between 0 and 5, defaults to 1"

    , Option "a" ["api-key"]
        (ReqArg
            (\arg opt -> return opt { optAPIKey = arg })
            "API KEY")
        "The API key to use."

    , Option "c" ["city"]
        (ReqArg
            (\arg opt -> return opt { optLocation = Fetch.setCity (optLocation opt) arg })
            "CITY")
        "The location whose weather forecast we\nare interested in.\n\
        \Use in conjunction with -C"

    , Option "C" ["country"]
        (ReqArg
            (\arg opt -> return opt { optLocation = Fetch.setCountry (optLocation opt) arg })
            "COUNTRY")
        "The location's country whose weather\nforecast we are interested in.\n\
        \Use in conjunction with -c"

    , Option "i" ["id"]
        (ReqArg
            (\arg opt -> return opt { optLocation = Fetch.CityID (readWrapper arg) })
            "ID")
        "The city's id whose weather forecast\nwe are interested in."

    , Option "" ["lat"]
        (ReqArg
            (\arg opt -> return opt { optLocation = Fetch.setLat
                (optLocation opt) (readWrapper arg) })
            "LATITUDE")
        "The location's latitude. Use in conjunction\nwith --lon"

    , Option "" ["lon"]
        (ReqArg
            (\arg opt -> return opt { optLocation = Fetch.setLon
                (optLocation opt) (readWrapper arg) })
            "LONGITUDE")
        "The location's longitude. Use in conjunction\nwith --lat"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output destination.\nDefault: stdout"

    , Option "f" ["format"]
        (ReqArg
            (\arg opt -> return opt { optFormat = readWrapper arg })
            "FORMAT")
        "Output format. Possible values are\n\
        \'Plaintext', 'ANSI' and 'Pango'. Default: ANSI"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let header = prg ++ " version 0.2\nUSAGE: " ++
                        prg ++ " [OPTION..]\nOPTIONS:" 
                hPutStrLn stderr (usageInfo header options)
                exitSuccess))
        "Show this help."
    ]

main :: IO ()
main = do
    -- handle args
    args <- getArgs
    let (actions, _, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optUnit = unit
                , optAPIKey = apiKey
                , optNumDays = n
                , optLocation = location
                , optOutput = output
                , optFormat = format
                } = opts

    -- get and process our data
    json <- Fetch.getRawWeatherdata apiKey unit location
    let weather = decode json :: Maybe Forecast
    if isJust weather
       -- le ugly hack below, think about a replacement
       then do let weather' = (fromMaybe (undefined) weather)
                   weather'' = weather' {
                       forecastData = take (n*8) (forecastData weather')}
               output (formatForecast format unit weather'')
       else output "An error with your data occured.\n\
                      \Please file a bugreport."
