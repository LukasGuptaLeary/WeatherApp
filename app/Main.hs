{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Network.HTTP.Simple
import Control.Monad (MonadPlus(mzero))
import Data.String (IsString(fromString))

type Latitude = Float
type Longitude = Float
type Temperature = Float

data Location = Location { lat :: Latitude, lng :: Longitude } deriving Show
newtype Geometry = Geometry { location :: Location } deriving Show
newtype GeoCodeResult = GeoCodeResult { geometry :: Geometry } deriving Show
newtype GeoCodeResults = GeoCodeResults { results :: [GeoCodeResult] } deriving Show

data TemperatureData = TemperatureData { temp :: Temperature, feels_like :: Temperature } deriving Show
newtype WeatherData = WeatherData { description :: String } deriving Show
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
        return $ GeoCodeResult { geometry = geometry' }
    parseJSON _ = mzero

instance FromJSON GeoCodeResults where
    parseJSON (Object v) = do
        results' <- v .: "results"
        return $ GeoCodeResults { results = results' }
    parseJSON _ = mzero

instance FromJSON TemperatureData where
    parseJSON (Object v) = do
        temp' <- v .: "temp"
        feels_like' <- v .: "feels_like"
        return $ TemperatureData { temp = temp', feels_like = feels_like' }
    parseJSON _ = mzero

instance FromJSON WeatherData where
    parseJSON (Object v) = do
        description' <- v .: "description"
        return $ WeatherData { description = description' }
    parseJSON _ = mzero

instance FromJSON WeatherResults where
    parseJSON (Object v) = do
        weather' <- v .: "weather"
        tempData' <- v .: "main"
        return $ WeatherResults { weather = head weather', tempData = tempData' }
    parseJSON _ = mzero

getGeoCodeResults :: String -> IO GeoCodeResults
getGeoCodeResults a = do
    request' <- parseRequest "GET https://maps.googleapis.com/maps/api/geocode/json"
    let request
            = setRequestQueryString [("key",Just "AIzaSyAQcuLCegTwRZMNKTebXmZ5FJuPebxzzys"), ("address", Just $ fromString a)]
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

main :: IO ()
main = do
    print "Enter your address to get your weather forcast:"
    address <- getLine
    geoCodeResults <- getGeoCodeResults address
    let yourLocation = location $ geometry $ head $ results geoCodeResults
    let yourLat = lat yourLocation
    let yourLong = lng yourLocation
    print $ "Your location is " ++ show yourLat ++ "," ++ show yourLong
    weatherResults <- getWeatherResults yourLat yourLong
    let yourTemp = temp $ tempData weatherResults
    let yourWeather = description $ weather weatherResults
    print $ "Your temperature is " ++ show yourTemp ++ "F and your forcast is " ++ yourWeather
