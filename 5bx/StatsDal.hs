module StatsDal where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)


import Database.HDBC (fromSql, SqlValue, quickQuery')
import Database.HDBC.MySQL (MySQLConnectInfo)
import Database.HDBC.Types (disconnect, IConnection)

import StatsModel

fromResults :: [SqlValue] -> Stats
fromResults [i, w, st, si, b, p, c] = Stats id weight stretches situps backexts pressups chart
      where f :: SqlValue -> Integer
            f x = read $ fromSql x
            id = f i
            weight = f w
            stretches = f st
            situps = f si
            backexts = f b
            pressups = f p
            chart = f c

currentStat :: (IConnection a) => a -> IO (Maybe Stats)
currentStat conn = query conn >>=
                    process

process :: [[SqlValue]] -> IO (Maybe Stats)
process [] = return Nothing
process rs = return (Just (fromResults $ head rs))


query :: (IConnection a) => a -> IO [[SqlValue]]
query conn = quickQuery' conn "SELECT id, weight, stretches, situps, backexts, pressups, chart FROM exercise_records ORDER BY id DESC LIMIT 1" []