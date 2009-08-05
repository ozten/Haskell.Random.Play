module Main where

import Control.Monad (msum, mzero)
import Data.List (isPrefixOf)
import Debug.Trace (trace)

import Happstack.Server (simpleHTTP, Conf(..), ServerPartT)
import Happstack.Server.HTTP.Types (rqURL)
import Happstack.Server.SimpleHTTP (askRq) 
import Happstack.Helpers (exactdir)

--import Text.Html ((<<), body, p, renderHtml)
--import Text.XHtml.Strict ((<<), body, p, renderHtml)
import Text.XHtml.Transitional ((<<), body, p, renderHtml)

import FiveBeeX

main = simpleHTTP (Conf 8080 Nothing) $ handleRequest

handleRequest :: ServerPartT IO String
handleRequest = msum [
                 exactdir "/" home
                 , do
                       rq <- askRq
                       if (rqURL rq) == "/weigh"
                         then return "Weighing In"
                         else mzero
                 , askRq >>= \rq ->
                    trace (rqURL rq) $
                    trace "hello zorld" $ 
                    --if isPrefixOf "/weigh" (rqURL rq)
                    if "/weigh" `beginsWith` (rqURL rq)
                      then
                        return (renderHtml (body << p <<  "Weighing In"))
                      else mzero                         
                ]
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