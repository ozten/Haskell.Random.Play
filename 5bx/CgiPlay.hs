-- ghc --make -o CgiPlay.cgi CgiPlay.hs 
import Network.CGI

cgiMain :: CGI CGIResult
cgiMain = output "Hello World!"

dumpEnv :: MonadCGI m => m CGIResult
dumpEnv = do v <- getVars
             output $ foldr (++) "\n" (map printEnv v)

printEnv :: (String, String) -> String
printEnv (n, v) = n ++ "=" ++ v ++ "<br>"


--printEnvM :: MonadCGI m => (String, String) -> m CGIResult
--printEnvM (n, v) = output $ n ++ "=" ++ v


main :: IO ()
main = runCGI (handleErrors dumpEnv)

-- output :: (MonadCGI m) => String -> m CGIResult
  	-- Defined in Network.CGI
