-- ghc --make -o main.cgi Main.hs 
import Network.CGI

import Data.List

import Controller

routeRequest :: CGI CGIResult
routeRequest = do path <- getVar "PATH_INFO"
                  method <- requestMethod
                  dispatch path method
               where dispatch :: (MonadCGI m, MonadIO m) => Maybe String -> String -> m CGIResult               
                     dispatch Nothing _ = output "Error reading PATH_INFO, is CGI hooked up?"
                     dispatch (Just p) m | "/page/create/" `isPrefixOf` p = Controller.createPage p m
                     dispatch (Just p) m | "/page/view/"   `isPrefixOf` p = Controller.viewPage p m
                     dispatch (Just p) m | "/page/edit/"   `isPrefixOf` p = Controller.editPage p m
                     dispatch (Just p) m | "/page/delete/" `isPrefixOf` p = Controller.deletePage p m
                     dispatch (Just p) m | "/pages/"       `isPrefixOf` p = Controller.listPages p m
                     dispatch (Just p) m = output p 

main :: IO ()
main = runCGI (handleErrors routeRequest)
--main = runCGI (handleErrors foobar) --routeRequest)