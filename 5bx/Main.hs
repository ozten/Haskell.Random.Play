{-
I wanted to host this on nearlyfreespeech, so I switched from Main.hs based on HappStack
to MainCgi.hs
-}
module Main where

import Control.Monad (mplus, msum, mzero)
import Control.Monad.Reader (liftM)
import Control.Monad.Trans

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

import Happstack.Server (look, fromData, FromData, simpleHTTP, Conf(..), toResponse, ServerPartT)
import Happstack.Server.FastCGI

import Happstack.Server.SURI (parse)

import Happstack.Server.HTTP.Types (redirect, rqMethod, rqURL, Response)
import Happstack.Server.SimpleHTTP (askRq, getData, RqData, ToMessage) 
import Happstack.Helpers (exactdir)

import Happstack.Server.Helpers (getData')

import Database.HDBC
import Database.HDBC.MySQL

import Text.XHtml.Transitional ((<<), (+++), body, h1, Html, p, strong)

import Config
import FiveBeeX
import StatsModel
import StatsDal
import WeightController
import WeightView

--main = handleSqlError $ trace "Starting up, try port 8080" (simpleHTTP (Conf 8080 Nothing) $ handleRequest)


simpleCGI :: (ToMessage a) => ServerPartT IO a -> IO ()
simpleCGI = runFastCGIConcurrent 10 . serverPartToCGI
 
main = simpleCGI handleRequest

handleRequest :: ServerPartT IO Response
handleRequest = msum [
                 exactdir "/" home
                 , do
                       rq <- askRq
                       if (rqURL rq) == "/experiment"
                         then
                           --return $ toResponse "huh"
                           getData' >>= weightForIt
                         else mzero
                 , exactdir "/currentWeight" currentWeight
                 , exactdir "/redir" doRedirect
                 , askRq >>= \rq ->
                    if "/weight" `beginsWith` (rqURL rq)
                      then
                        if "GET" == show (rqMethod rq)
                          then
                            doWeight
                          else
                            recordWeight rq
                      else mzero                         
                ]
                
data WeightInfo = WeightInfo { theWeight :: String }
instance FromData WeightInfo where
    fromData = liftM WeightInfo (look "weight" `mplus` return "0")
    
currentWeight :: ServerPartT IO Response
currentWeight = connectDb >>= (\conn ->
                    liftIO (currentStat conn) >>= showStats)
                where
                  connectDb = liftIO $ connectMySQL defaultMySQLConnectInfo { mysqlHost = host, mysqlDatabase = database, mysqlUser = username, mysqlPassword = password, mysqlUnixSocket = unixSocket}
                  showStats :: Maybe Stats -> ServerPartT IO Response
                  showStats stats = return $ toResponse $ display stats

display :: Maybe Stats -> Html
display stats = body <<
    h1 << "Current Weight" +++
    p << ("You current weight is " +++
        strong << doStats stats)
    where doStats :: Maybe (Stats) -> String
          doStats Nothing = "unknown"
          doStats stat = show (weight (fromJust stat))
    
weightForIt :: String -> ServerPartT IO Response
weightForIt aWeight = return $ toResponse aWeight

doRedirect :: (Monad m) => m Response
doRedirect = return (Happstack.Server.HTTP.Types.redirect 302 (fromJust (parse "http://oface.ubuntu:8080/")) (toResponse "later"))

-- Exactly the same as Data.List.isPrefixOf                
beginsWith :: String -> String -> Bool
beginsWith [] _ = True
beginsWith pre (x:xs) | (head pre) == x = beginsWith (tail pre) xs
beginsWith _ _ = False

{-                 
exactdir :: (Monad m) => String -> ServerPartT m a -> ServerPartT m a
exactdir staticPath = spsIf (\rq -> rqURL rq == staticPath)

spsIf :: (Monad m) => (Request -> Bool) -> ServerPartT m a -> ServerPartT m a
spsIf p sps = do
  rq <- askRq
  if p rq
    then sps
    else mzero
-}

                

-- msum [action, action]
-- each action does something or returns nothing
-- an action
-- dir "