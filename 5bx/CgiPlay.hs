-- ghc --make -o CgiPlay.cgi CgiPlay.hs 
import Network.CGI

cgiMain :: CGI CGIResult
cgiMain = output "Hello World!"

main :: IO ()
main = runCGI (handleErrors cgiMain)
