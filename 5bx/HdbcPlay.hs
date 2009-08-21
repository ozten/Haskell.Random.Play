module HdbcPlay where

import Database.HDBC
import Database.HDBC.MySQL
import Control.Monad (liftM)
import Config

main = do conn <- connectMySQL defaultMySQLConnectInfo {
                    mysqlHost       = host,
                    mysqlDatabase   = database,
                    mysqlUser       = username,
                    mysqlPassword   = password,
                    mysqlUnixSocket = unixSocket
                  }
          -- run for executing INSERT, CREATE, DELETE statements
          rows <- run conn "INSERT INTO exercise_records (weight, stretches, situps, backexts, pressups, chart, created, updated) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)" [toSql "200", toSql "0", toSql "0", toSql "0", toSql "0", toSql "1"]
          --forM_ rows $ \row -> putStrLn $ show row          
          putStrLn $ show rows
          --execute for INSERT CREATE DELETE prepared statements
          --stmt <- prepare conn "SELECT * FROM exercise_records"          
          --execute stmt []
          
          rs <- quickQuery' conn "SELECT id, weight, stretches, situps, backexts, pressups, chart FROM exercise_records" []
          -- trial and error... why is this
          -- printRs rs
          let a = map printRow rs
          putStrLn $ head a 
          -- not like this
          -- a  <- fmap printRs rs
          -- or
          -- printRs :: IO [(String, SqlValue)] -> IO ()
          commit conn
          disconnect conn
{- 
printRs :: IO [[(String, SqlValue)]] -> IO ()
printRs

printRs :: [[SqlValue]] -> IO ()
printRs x = putStrLn $ "First value: " ++ (fromSql (head (head x)))
-}
--printAllRs :: [SqlValue] -> Integer

printRow :: [SqlValue] -> String
printRow [id, weight, stretches, situps, backexts, pressups, chart] = f id ++ f weight ++ f stretches ++ f backexts ++ f pressups ++ f chart
    where f x = " " ++ fromSql x