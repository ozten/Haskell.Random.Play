module WeightController
    (getCurrent, dispatchCurrent)
    where
import Prelude

import Network.CGI
import Control.Monad (liftM)

import StatsDal
import StatsModel
import WeightView

{-
data Services = Services {
    get :: (MonadCGI m, MonadIO m) => m CGIResult,
    head :: (MonadCGI m, MonadIO m) => m CGIResult,
    
    post :: (MonadCGI m, MonadIO m) => m CGIResult,
    put :: (MonadCGI m, MonadIO m) => m CGIResult,
    delete :: (MonadCGI m, MonadIO m) => m CGIResult,
    
    options :: (MonadCGI m, MonadIO m) => m CGIResult,
    trace :: (MonadCGI m, MonadIO m) => m CGIResult,
    connect :: (MonadCGI m, MonadIO m) => m CGIResult
}
provides = Services {
    get     = unsupportedMethod,
    head    = unsupportedMethod,
    post    = unsupportedMethod, 
    put     = unsupportedMethod,
    delete  = unsupportedMethod,
    options = unsupportedMethod,
    trace   = unsupportedMethod,
    connect = unsupportedMethod
}
-}

url = "/5bx/weight/current"

dispatchCurrent :: (MonadCGI m, MonadIO m) => String -> String -> m CGIResult
dispatchCurrent path m | m == "POST" = postCurrent
dispatchCurrent path m | m == "GET" = getCurrent
dispatchCurrent path m = error $ path ++ " does not support the method:" ++ m ++ " invalid request"
        
getCurrent :: (MonadCGI m, MonadIO m) => m CGIResult
getCurrent =
    do stats <- liftIO getCurrentStat
       display stats
    where display Nothing = output $ currentWeightView "0"
          display (Just stat) = output $ currentWeightView $ show $ weight stat

postCurrent :: (MonadCGI m, MonadIO m) => m CGIResult
postCurrent =
    do weight <- readInput "weight"
       save weight
       _redirect url
    where
      --save :: (MonadCGI m, MonadIO m) => (Maybe Integer) -> m CGIResult
      save :: (MonadIO m) => (Maybe Integer) -> m Integer
      save Nothing = error "Expected an input weight"
      save (Just w) = liftIO (recordWeight w)
      
    {- connectDb >>= (\conn ->
                           liftIO $ saveStats conn (stats (toInt weight))) >>                           
                      --return ( toResponse "oh ya")
                      return (redirect 302 (fromJust (parse "http://oface.ubuntu:8080/weight")) (toResponse "later"))
                      
                   where connectDb = liftIO $ connectMySQL defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}
                         getWeight :: [(String, Input)] -> String
                         getWeight d = B.unpack ( inputValue $ snd $ head $ filter (\i -> (fst i == "weight")) d)
                         toInt :: String -> Integer
                         toInt s = trace s (read s)
                         stats :: Integer -> Stats
                         stats weight = Stats (-1) weight 0 0 0 0 1
-}          

_redirect :: (MonadCGI m, MonadIO m) => String -> m CGIResult
_redirect location =
    do setStatus 302 "Found"
       setHeader "Location" location
       outputNothing