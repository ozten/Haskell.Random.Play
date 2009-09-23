{-
INSERT INTO stories (id, title, authors) VALUES ('couldbewild', 'Where The Wild Things Could Be...', '');
-}
module Model where

import Database.HDBC (commit, disconnect, fromSql, handleSqlError, run, SqlValue, toSql, quickQuery')
import Database.HDBC.MySQL (MySQLConnectInfo, mysqlHost, connectMySQL, defaultMySQLConnectInfo, mysqlDatabase, mysqlUser, mysqlPassword, mysqlUnixSocket)
import Database.HDBC.Types (disconnect, IConnection)

import Config

data Page = Page {
    page_id :: String,
    story_fk_id :: String,
    prose :: String
} deriving (Show, Eq)

reviseProse :: Page -> String -> Page
reviseProse page newProse = Page (page_id page) (story_fk_id page) newProse

data Story = Story {
    story_id :: String,
    title :: String,
    authors :: String
} deriving (Show, Eq)

--loadStory :: [[SqlValue]] -> [(String, String, String)]
loadPage :: [[SqlValue]] -> [Page]
loadPage rows | length rows == 0 = []
loadPage rows | length rows == 1 =
    row2Story (head rows) : []
    where
        --row2Story :: [SqlValue] -> (String, String, String)
        row2Story :: [SqlValue] -> Page
        row2Story [i, t, a] = Page (fromSql i) (fromSql t) (fromSql a)
loadPage rows = head (loadPage [(head rows)]) : loadPage (tail rows)

-- (IConnection a) => a ->
-- create a page
createPage ::  String -> String -> IO Integer
createPage page_id story_id =
    do conn <- connect
       rs <- run conn sql params
       commit conn
       disconnect conn
       return rs
    where
       sql = "INSERT INTO pages (id, stories_id, prose) VALUES (?, ?, '')"
       params = [toSql page_id, toSql story_id]
       
-- update a page
updatePage :: Page -> IO Integer
updatePage page =
    do conn <- connect
       rs <- run conn sql params
       commit conn
       disconnect conn
       return rs
    where
       sql = "UPDATE pages SET prose = ? WHERE stories_id = ? AND id = ?"
       params = [toSql (prose page), toSql (story_fk_id page), toSql (page_id page)]
       
deletePage :: Page -> IO Integer
deletePage page =
    do conn <- connect
       rs <- run conn sql params
       commit conn
       disconnect conn
       return rs
    where
       sql = "DELETE FROM pages WHERE stories_id = ? AND id = ?"
       params = [toSql (story_fk_id page), toSql (page_id page)]
       
connect = connectMySQL $ defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}

-- create a story

-- get a page

getPage :: String -> String -> IO [Page]
getPage page_id story_id =
    do conn <- connect
       rs <- quickQuery' conn sql params
       commit conn
       disconnect conn
       return $ loadPage rs
    where
       sql = "SELECT id, stories_id, prose FROM pages where id = ? AND stories_id = ?"
       params = [toSql page_id, toSql story_id]
       
getPages :: String -> IO [Page]
getPages story_id =
    do conn <- connect
       rs <- quickQuery' conn sql params
       commit conn
       disconnect conn
       return $ loadPage rs
    where
       sql = "SELECT id, stories_id, prose FROM pages WHERE stories_id = ?"
       params = [toSql story_id]

-- update a page
{-
getS story_id =
    do conn <- connect
       rs <- quickQuery' conn sql params
       disconnect conn
       --return $ (fromSql $ head $ head rs) ++ (fromSql $ head $ tail $ head rs)
       return $ loadStory rs
    where
       sql = "SELECT id, title, authors FROM stories where id = ?"
       params = [toSql story_id]
-}

-- delete a page
getStory :: String -> IO [Story]
getStory story_id =
    do conn <- connect
       rs <- quickQuery' conn sql params
       disconnect conn
       return $ loadStory rs
    where
       sql = "SELECT id, title, authors FROM stories where id = ?"
       params = [toSql story_id]

loadStory :: [[SqlValue]] -> [Story]
loadStory rows | length rows == 0 = []
loadStory rows | length rows == 1 =
    row2Story (head rows) : []
    where
        row2Story :: [SqlValue] -> Story
        row2Story [i, t, a] = 
            Story (fromSql i) (fromSql t) (fromSql a)
loadStory rows = head (loadStory [(head rows)]) : loadStory (tail rows)
          
updateStory :: Story -> IO Integer
updateStory story =
    do conn <- connect
       rs <- run conn sql params
       commit conn
       disconnect conn
       return rs
    where
       sql = "UPDATE stories SET title = ?, authors = ? WHERE id = ?"
       params = [toSql (title story), toSql (authors story), toSql (story_id story)]

reviseStory :: Story -> String -> String -> Story
reviseStory story title authors = Story (story_id story) title authors

-- delete a story
deleteStory :: Story -> IO Integer
deleteStory story =
    do conn <- connect
       rs <- run conn sql params
       commit conn
       disconnect conn
       return rs
    where
       sql = "DELETE FROM stories WHERE id = ?"
       params = [toSql (story_id story)]