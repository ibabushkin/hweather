# Maintainer note
This is deprecated and unmaintained. Use something else instead. I quite enjoy
http://wttr.in/.

# hweather - a weather app 
Hweather is a weather app that uses [openweathermap.org](http://www.openweathermap.org)
to fetch weather data for any location you need. It translates all the options of
the API to easy-to-use and clean command-line arguments. To use it, you'll need
an API key, which can be obtained [here](http://openweathermap.org/appid).

## Features
The below help message summarizes the features better than I could. Oh wait.
```
hweather version 0.3
USAGE: hweather [OPTION..]
OPTIONS:
  -u UNIT     --unit=UNIT            Units to use. Possible values are:
                                     'Default' (°K, m/s), 'Metric' (°C, m/s)
                                     and 'Imperial' (°F, mph)
  -n NUMBER   --numdays=NUMBER       Number of days to display the forecast for.
                                     Should be between 0 and 5, defaults to 1
                                     For greater precision, use -N
  -N NUMBER   --numintervals=NUMBER  Number of 3h intervals to display the forecast
                                     for. Should be between 0 and 40, defaults to 8
                                     For lesser precision, use -n
  -a API KEY  --api-key=API KEY      The API key to use.
  -c CITY     --city=CITY            The location whose weather forecast we
                                     are interested in.
                                     Use in conjunction with -C
  -C COUNTRY  --country=COUNTRY      The location's country whose weather
                                     forecast we are interested in.
                                     Use in conjunction with -c
  -i ID       --id=ID                The city's id whose weather forecast
                                     we are interested in.
              --lat=LATITUDE         The location's latitude. Use in conjunction
                                     with --lon
              --lon=LONGITUDE        The location's longitude. Use in conjunction
                                     with --lat
  -o FILE     --output=FILE          Output destination.
                                     Default: stdout
  -f FORMAT   --format=FORMAT        Output format. Possible values are
                                     'Plaintext', 'ANSI' and 'Pango'. Default: ANSI
  -h          --help                 Show this help.
```

## Installation
Since hweather is written in Haskell, you will need GHC and cabal-install on your
system. The installation is done by 
```
$ git clone https://www.github.com/ibabushkin/hweather
$ cd hweather
$ cabal build
$ cabal install
```
Now, you should have an executable named `hweather` in `~/.cabal/bin/`. Add that
directory to your path if you wish to call it directly.
