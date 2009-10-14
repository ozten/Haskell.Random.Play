module StatsDal where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import Debug.Trace (putTraceMsg, trace)

import Database.HDBC (commit, disconnect, fromSql, handleSqlError, run, SqlValue, toSql, quickQuery')
import Database.HDBC.MySQL (MySQLConnectInfo, mysqlHost, connectMySQL, defaultMySQLConnectInfo, mysqlDatabase, mysqlUser, mysqlPassword, mysqlUnixSocket)
import Database.HDBC.Types (disconnect, IConnection)

import Config
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

getCurrentStat :: IO (Maybe Stats)
getCurrentStat = do conn <- connect
                    currentStat conn
        where connect = connectMySQL $ defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}


currentStat :: (IConnection a) => a -> IO (Maybe Stats)
currentStat conn = query conn >>=
                   process
                where
                   sql = "SELECT id, weight, stretches, situps, backexts, pressups, chart FROM exercise_records ORDER BY id DESC LIMIT 1"
                   query :: (IConnection a) => a -> IO [[SqlValue]]
                   query conn = quickQuery' conn sql []

xsaveStats :: (IConnection a) => a -> Stats -> IO Integer
xsaveStats conn stats = {-let rs = run conn sql params
                       in commit conn >>
                          --disconnect conn >>
                          rs-}
                       do rs <- run conn sql params
                          commit conn
                          putTraceMsg (show rs)
                          rs2 <- quickQuery' conn "SELECT id, weight FROM exercise_records" []
                          --liftIO (printRes rs2)
                          let stringRows = map convRow rs2
                          mapM_ putStrLn stringRows
                          --putTraceMsg (show stats)
                          disconnect conn
                          commit conn
                          return rs
                where
                    sql = "INSERT INTO exercise_records (weight, stretches, situps, backexts, pressups, chart, created, updated) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
                    params = [toSql $ weight stats, toSql $ stretches stats, toSql $ situps stats,
                              toSql $ backexts stats, toSql $ situps stats, toSql $ chart stats]
                    convRow :: [SqlValue] -> String 
                    convRow [sqlId, sqlDesc] = 
                        show intid ++ ": " ++ desc 
                        where intid = (fromSql sqlId)::Integer 
                              desc = case fromSql sqlDesc of 
                                         Just x -> x 
                                         Nothing -> "NULL" 
                    convRow x = fail $ "Unexpected result: " ++ show x

recordWeight :: Integer -> IO Integer
recordWeight weight =
    do conn <- connection
       saveStats conn (stats weight)
    where
        connection = connectMySQL $ defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}
        stats weight = Stats (-1) weight 0 0 0 0 1
                   
saveStats :: (IConnection a) => a -> Stats -> IO Integer
saveStats conn stats = {-let rs = run conn sql params
                       in commit conn >>
                          --disconnect conn >>
                          rs-}
                       do rs <- run conn sql params
                          commit conn
                          --putTraceMsg (show stats)
                          --disconnect conn
                          return rs
                where
                    sql = "INSERT INTO exercise_records (weight, stretches, situps, backexts, pressups, chart, created, updated) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
                    params = [toSql $ weight stats, toSql $ stretches stats, toSql $ situps stats,
                              toSql $ backexts stats, toSql $ situps stats, toSql $ chart stats]

process :: [[SqlValue]] -> IO (Maybe Stats)
process [] = return Nothing
process rs = return (Just (fromResults $ head rs))


