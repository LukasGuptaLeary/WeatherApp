{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson
import Network.HTTP.Simple
import Control.Monad (MonadPlus(mzero))
import Data.String (IsString(fromString))

type Latitude = Float
type Longitude = Float
type Temperature = Float
type Address = String
type Forecast = String

data Location = Location { lat :: Latitude, lng :: Longitude } deriving Show
newtype Geometry = Geometry { location :: Location } deriving Show
data GeoCodeResult = GeoCodeResult { geometry :: Geometry, formattedAddress :: Address } deriving Show
newtype GeoCodeResults = GeoCodeResults { results :: [GeoCodeResult] } deriving Show

data TemperatureData = TemperatureData { temp :: Temperature, feelsLike :: Temperature } deriving Show
newtype WeatherData = WeatherData { forecast :: Forecast } deriving Show
data WeatherResults = WeatherResults { weather :: WeatherData, tempData :: TemperatureData } deriving Show

instance FromJSON Location where
    parseJSON (Object v) = do
        lat' <- v .: "lat"
        lng' <- v .: "lng"
        return $ Location { lat = lat', lng = lng' }
    parseJSON _ = mzero

instance FromJSON Geometry where
    parseJSON (Object v) = do
        location' <- v .: "location"
        return $ Geometry { location = location' }
    parseJSON _ = mzero

instance FromJSON GeoCodeResult where
    parseJSON (Object v) = do
        geometry' <- v .: "geometry"
        formattedAddress' <- v .: "formatted_address"
        return $ GeoCodeResult { geometry = geometry', formattedAddress = formattedAddress' }
    parseJSON _ = mzero

instance FromJSON GeoCodeResults where
    parseJSON (Object v) = do
        results' <- v .: "results"
        return $ GeoCodeResults { results = results' }
    parseJSON _ = mzero

instance FromJSON TemperatureData where
    parseJSON (Object v) = do
        temp' <- v .: "temp"
        feelsLike' <- v .: "feels_like"
        return $ TemperatureData { temp = temp', feelsLike = feelsLike' }
    parseJSON _ = mzero

instance FromJSON WeatherData where
    parseJSON (Object v) = do
        forecast' <- v .: "description"
        return $ WeatherData { forecast = forecast' }
    parseJSON _ = mzero

instance FromJSON WeatherResults where
    parseJSON (Object v) = do
        weather' <- v .: "weather"
        tempData' <- v .: "main"
        return $ WeatherResults { weather = head weather', tempData = tempData' }
    parseJSON _ = mzero

getGeoCodeResults :: Address -> IO GeoCodeResults
getGeoCodeResults a = do
    request' <- parseRequest "GET https://maps.googleapis.com/maps/api/geocode/json"
    let request
            = setRequestQueryString [   ("key",Just "AIzaSyAQcuLCegTwRZMNKTebXmZ5FJuPebxzzys")
                                        , ("address", Just $ fromString a)]
            request'
    response <- httpJSON request :: IO (Response GeoCodeResults)
    return $ getResponseBody response

getWeatherResults :: Float -> Float -> IO WeatherResults
getWeatherResults lt lo = do
    request' <- parseRequest "GET https://api.openweathermap.org/data/2.5/weather"
    let request
            = setRequestQueryString [   ("appid",Just "f68bb5180457645bf9e1d0a8e6e6b0ba")
                                        , ("lat", Just $ fromString $ show lt)
                                        , ("lon", Just $ fromString $ show lo)
                                        , ("units", Just "imperial")]
            request'
    response <- httpJSON request :: IO (Response WeatherResults)
    return $ getResponseBody response

getResultsForAddress :: IO Address -> (Address -> IO GeoCodeResults) -> IO GeoCodeResults
getResultsForAddress a r = a >>= r

getLocationFromResult :: GeoCodeResult -> Location
getLocationFromResult r = location $ geometry r

getResultArray :: IO Address -> IO [GeoCodeResult]
getResultArray a = results <$> getResultsForAddress a getGeoCodeResults

selectLocation :: IO [GeoCodeResult] -> IO Location
selectLocation r = do
    geoCodeResults <- r
    let options = zip [1 .. length geoCodeResults] $ map formattedAddress geoCodeResults
    print "Select your address:"
    mapM_ (\x -> print $ show (fst x) ++ ") " ++ snd x) options
    addressSelection :: Int <- readLn
    if addressSelection > 0 && addressSelection <= length geoCodeResults
        then return $ getLocationFromResult $ last $ take addressSelection geoCodeResults
        else selectLocation r

getLocation :: IO Location
getLocation = do
    print "Search for your address to get your weather forcast:"
    geoCodeResults <- getResultArray getLine
    case geoCodeResults of
        [] -> getLocation
        _  ->  selectLocation $ pure geoCodeResults


main :: IO ()
main = do
    yourLocation <- getLocation
    let yourLat = lat yourLocation
    let yourLong = lng yourLocation
    print $ "Your location is " ++ show yourLat ++ "," ++ show yourLong
    weatherResults <- getWeatherResults yourLat yourLong
    let yourTemp = temp $ tempData weatherResults
    let yourForecast = forecast $ weather weatherResults
    print $ "Your temperature is " ++ show yourTemp ++ "F and your forcast is " ++ yourForecast
