module Main where

import Control.Monad (msum, mzero)
import Control.Monad.Reader (liftM)

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

import Happstack.Server (simpleHTTP, Conf(..), toResponse, ServerPartT)

import Happstack.Server.SURI (parse)

import Happstack.Server.HTTP.Types (redirect, rqMethod, rqURL, Response)
import Happstack.Server.SimpleHTTP (askRq, getData) 
import Happstack.Helpers (exactdir)

import Happstack.Server.Helpers (getData')

import FiveBeeX

main = simpleHTTP (Conf 8080 Nothing) $ handleRequest

handleRequest :: ServerPartT IO Response
handleRequest = msum [
                 exactdir "/" home
                 , do
                       rq <- askRq
                       if (rqURL rq) == "/weigh"
                         then return $ toResponse "Weighing In"
                         else mzero
                 , do
                       rq <- askRq
                       if (rqURL rq) == "/experiment"
                         then
                           --return $ toResponse "huh"
                           getData' >>= weightForIt
                         else mzero
                 , exactdir "/redir" doRedirect
                 , askRq >>= \rq ->
                    trace (show (rqMethod rq)) $
                    trace "hello zorld" $
                    --if isPrefixOf "/weigh" (rqURL rq)
                    if "/weight" `beginsWith` (rqURL rq)
                      then
                        if "GET" == show (rqMethod rq)
                          then
                            weight
                          else
                            record_weight rq
                      else mzero                         
                ]
weightForIt :: Integer -> ServerPartT IO Response
weightForIt weight = return $ toResponse "hey" --( show weight )

doRedirect :: (Monad m) => m Response
doRedirect = return (redirect 302 (fromJust (parse "http://oface.ubuntu:8080/")) (toResponse "later"))

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