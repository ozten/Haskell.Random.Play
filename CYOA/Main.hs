-- ghc --make -o main.cgi Main.hs 
import Network.CGI

import Data.List

import Auth
import Controller


routeRequest :: CGI CGIResult
routeRequest = do path <- getVar "PATH_INFO"
                  method <- requestMethod
                  mip <- getVar "REMOTE_ADDR"
                  dispatch path method mip
               where dispatch :: (MonadCGI m, MonadIO m) => Maybe String -> String -> Maybe String -> m CGIResult               
                     dispatch Nothing _ _ = output "Error reading PATH_INFO, is CGI hooked up?"
                     dispatch _ _ Nothing = output "Error reading REMOTE_ADDR, is CGI hooked up?"
                     dispatch (Just p) m (Just ip) | "/page/create/" `isPrefixOf` p = mustBeAnEditor ip $ Controller.createPage p m
                     dispatch (Just p) m (Just ip)  | "/page/view/"   `isPrefixOf` p = Controller.viewPage p m
                     dispatch (Just p) m (Just ip) | "/page/edit/"   `isPrefixOf` p = mustBeAnEditor ip $ Controller.editPage p m
                     dispatch (Just p) m (Just ip) | "/page/delete/" `isPrefixOf` p = mustBeAnEditor ip $ Controller.deletePage p m
                     dispatch (Just p) m (Just ip) | "/pages/"       `isPrefixOf` p = Controller.listPages p m
                     dispatch (Just p) m (Just ip) | "/story/edit"   `isPrefixOf` p = mustBeAnEditor ip $ Controller.editStory p m
                     dispatch (Just p) m (Just ip) = output p

main :: IO ()
main = runCGI (handleErrors routeRequest)
--main = runCGI (handleErrors foobar) --routeRequest)