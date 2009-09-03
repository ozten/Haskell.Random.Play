-- ghc -package fastcgi -threaded --make -o FCgiPlay.fcgi FCgiPlay.hs 

{-
import Control.Concurrent
import System.Posix.Process (getProcessID)

import Network.FastCGI

test :: CGI CGIResult
test = do setHeader "Content-type" "text/plain"
          pid <- liftIO getProcessID
          threadId <- liftIO myThreadId
          let tid = concat $ drop 1 $ words $ show threadId
          output $ unlines [ "Process ID: " ++ show pid,
                             "Thread ID:  " ++ tid]

main = runFastCGIConcurrent' forkIO 10 test
-}
import Happstack.Server.FastCGI
import Happstack.Server.SimpleHTTP (askRq, getData, RqData, ToMessage)
import Happstack.Server (look, fromData, FromData, simpleHTTP, Conf(..), toResponse, ServerPartT)

simpleCGI :: (ToMessage a) => ServerPartT IO a -> IO ()
simpleCGI = runFastCGIConcurrent 10 . serverPartToCGI
 
main = simpleCGI handleRequest

handleRequest = return $ toResponse "Howdy From Happstack on FastCGI"