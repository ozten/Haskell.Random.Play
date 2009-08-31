module WeightController where

import Control.Monad (mplus, msum, mzero)
import Control.Monad.Reader (liftM)
import Control.Monad.Trans

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
--import Data.ByteString (ByteString, read)
import qualified Data.ByteString.Lazy.Char8 as B


import Happstack.Server (look, fromData, FromData, Request, simpleHTTP, Conf(..), toResponse, ServerPartT)

import Happstack.Server.SURI (parse)

import Happstack.Server.HTTP.Types (redirect, inputValue, Input, rqMethod, rqURL, Response)
import Happstack.Server.SimpleHTTP (askRq, getData, rqInputs, ServerMonad, RqData) 



import Happstack.Helpers (exactdir)

import Happstack.Server.Helpers (getData')


import Database.HDBC
import Database.HDBC.MySQL

import Config
import StatsModel
import StatsDal
import WeightView

xdoWeight :: (ServerMonad m) => m Response
xdoWeight = showWeight "275"

doWeight :: ServerPartT IO Response
doWeight = connectDb >>= (\conn ->
                    liftIO (currentStat conn) >>= showStats)                    
                where
                  connectDb = liftIO $ connectMySQL defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}
                  showStats :: Maybe Stats -> ServerPartT IO Response
--                  showStats _ = showWeight ""
                  showStats Nothing = showWeight ""
                  showStats stats =  showWeight (show (weight (fromJust stats)))

-- Left off at http://tutorial.happstack.com/tutorial/get-and-post
recordWeight :: Request -> ServerPartT IO Response
recordWeight req = let weight = getWeight (rqInputs req)
                   in trace ("Seeing " ++ weight) connectDb >>= (\conn ->
                           liftIO $ saveStats conn (stats (toInt weight))) >>                           
                      return ( toResponse "oh ya")
                   where connectDb = liftIO $ connectMySQL defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}
                         getWeight :: [(String, Input)] -> String
                         getWeight d = B.unpack ( inputValue $ snd $ head $ filter (\i -> (fst i == "weight")) d)
                         toInt :: String -> Integer
                         toInt s = trace s (read s)
                         stats :: Integer -> Stats
                         stats weight = Stats (-1) weight 0 0 0 0 1
                         
segFault = let connectDb =  connectMySQL defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}
               s = (Stats (-1) 279 0 0 0 0 1)
           in do conn <- connectDb
                 --rs <- saveStats conn s
                 xsaveStats conn s
                 commit conn
                 --disconnect conn
                          {-getWeight d = do
                              maybeTheW <- getData'
                              case maybeTheW of
                                  Nothing -> "Error"
                                  Just aWeight -> theWeight aWeight-}
                          
{- http://totherme.livejournal.com/3674.html
It's all about the function types
Instead of focusing on runtime-time imperative behavior
what are the types of the functions you'll need?

Focus on data types and function type signatures

write your qucikCheck properties in parrallel

Stub stuff out

Once you have no stubs left, run quick check
-}