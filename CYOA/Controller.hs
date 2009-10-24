module Controller where
import Network.CGI
import Data.List (elemIndex)

import Auth
import Model
import View

{-
Our Page controllers

TODO left off here... Controllers can't be pure... They must be infected
with the CGI monad. Why? so we can do DB IO and other CGI IO.
Pull requestMethod logic out of Main.hs into this file...
Once that is working... lift Model.getStory into storyIsValid
-}
createPage, viewPage, editPage, deletePage, listPages :: (MonadCGI m, MonadIO m) => String -> String -> m CGIResult
createPage path method =
       case method of
           "POST" -> do stories <- liftIO $ Model.getStory story_id
                        p <- getInput "page_name"
                        case p of
                            Nothing -> output "Error, missing post parameter page_name"
                            Just page_id ->
                                case (length stories) of
                                    1 -> do pages <- liftIO $ Model.getPage page_id story_id
                                            case (length pages) of
                                                0 -> do action <- getInput "action"
                                                        case action of
                                                            Just label | label == cancelLabel -> do r <- getInput "referer"
                                                                                                    redirect r
                                                            Just label | label == saveLabel -> processPageUpdate page_id story_id                                                            
                                                -- start page always exists, because it's created when the story is created...
                                                _ -> _redirect $ "/page/view/" ++ story_id ++ "/" ++ page_id -- TODO error message "Page already exists"
                                    _ -> output $ "Couldn't find the story " ++ story_id
           _ -> do r <- getVar "HTTP_REFERER"
                   case r of
                       Nothing -> output $ View.createPageForm story_id ("/pages/" ++ story_id ++ "/start")
                       Just referer -> output $ View.createPageForm story_id referer
           
    where
        story_id :: String
        story_id = drop (length "/page/create/") path
        processPageUpdate page_id story_id =
            do results <- liftIO $ Model.createPage page_id story_id
               case results of
                   1 -> _redirect $ "/page/edit/" ++ story_id ++ "/" ++ page_id
                   _ ->  output "Story already exists? Some other Error?"
        redirect Nothing = output "Error, expected referer in form data"
        redirect (Just referer) = _redirect referer

viewPage path method =
    let parts = splitPath path "/page/view/"
        story_id = fst $ parts
        page_id = snd $ parts
    in  do pages <- liftIO $ Model.getPage page_id story_id
           stories <- liftIO $ Model.getStory story_id
           case (length pages) of
               1 -> let page = head pages
                    in case (length stories) of
                            1 -> case page_id of
                                 "start" -> output $ ( View.viewStartPage (head stories) page )
                                 _ -> output $ View.viewPage (head stories) page                                      
                            _ -> output ("Error: Unknown Story " ++ story_id)
               0 -> output $ "Error the page '" ++ path ++ "' doesn't exist yet." -- TODO 404

splitPath :: String -> String -> (String, String)
splitPath path urlPrefix =
            let both = drop (length urlPrefix) path
                pos = '/' `elemIndex` both
            in case pos of
               Nothing -> (both, "")
               Just p -> (take p both, drop (p + 1) both)


editPage path method =
    let parts = splitPath path "/page/view/"
        story_id = fst $ parts
        page_id = snd $ parts
    in do pages <- liftIO $ Model.getPage page_id story_id
          case (length pages) of
               1 -> let page = head pages
                    in case method of
                           "POST" -> processUpdatePageOrRedirect page
                           _ -> output $ View.editPage page
               0 -> output "Error that page doesn't exist" -- TODO 404

editStory path method =
    let story_id = drop (length "/story/edit/") path        
    in do stories <- liftIO $ Model.getStory story_id
          case (length stories) of
               1 -> let story = head stories
                    in case method of
                           "POST" -> processUpdateStoryOrRedirect story
                           _ -> output $ View.editStory story
               0 -> output "Error that story doesn't exist" -- TODO 404

processUpdateStoryOrRedirect story =
    do action <- getInput "action"
       case action of
           Just label | label == cancelLabel -> _redirect $ "/page/view/" ++ (story_id story) ++ "/start"
           Just label | label == saveLabel -> processStoryUpdate story
           _ -> output "Error unknown action"

processStoryUpdate story =
    do title <- getInput "title"
       authors <- getInput "authors"
       case title of
           Just t ->
               case authors of
                   Just a -> do liftIO $ Model.updateStory $ reviseStory story t a
                                _redirect $ "/page/view/" ++ (story_id story) ++ "/start"
                   Nothing -> output "Error missing author input, cannot update Story"
           Nothing -> output "Error missing title input, cannot update Story"
           
processUpdatePageOrRedirect page =
    do action <- getInput "action"
       case action of
           Just label | label == cancelLabel -> _redirect $ "/page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page)
           Just label | label == saveLabel -> processUpdate page
           _ -> output "Error unknown action"


processUpdate page =
    do prose <- getInput "prose"
       case prose of
           Just p ->
                do liftIO $ Model.updatePage $ reviseProse page p
                   _redirect $ "/page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page)
           Nothing -> output "Error missing prose element, cannot update Page"
           
deletePage path method =
    let parts = splitPath path "/page/delete/"
        story_id = fst $ parts
        page_id = snd $ parts
    in do pages <- liftIO $ Model.getPage page_id story_id
          case (length pages) of
               1 -> let page = head pages
                    in case method of
                           "POST" -> processDeleteOrRedirect page
                           _ -> output $ View.viewDeleteForm page
               0 -> output "Error that page doesn't exist" -- TODO 404
               
processDeleteOrRedirect page =
    do action <- getInput "action"
       case action of
           Just a | a == deleteLabel ->
               do liftIO $ Model.deletePage page
                  _redirect $ "/pages/" ++ (story_fk_id page)
           _ -> _redirect $ "/page/edit/" ++ (story_fk_id page) ++ "/" ++ (page_id page)


listPages path method =
    let story_id = drop (length "/pages/") path
    in do pages <- liftIO $ Model.getPages story_id
          output ( View.listPages story_id pages )



_redirect :: (MonadCGI m) => String -> m CGIResult
_redirect location =
    do setStatus 302 "Found"
       setHeader "Location" location
       outputNothing