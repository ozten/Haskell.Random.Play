module View where

import Text.XHtml.Transitional
import Data.String.Utils (replace) -- MissingH
import Model

saveLabel = "Save"
cancelLabel = "Cancel"
deleteLabel = "Delete"

createPageForm story referer =
    template ("Create A New Page for " ++ story)
             (h1 << ("Create A New Page for " ++ story) +++
             form ! [htmlAttr "id" << "create-page-form", htmlAttr "method" << "post", htmlAttr "action" << ("/page/create/" ++ story)] <<                 
                 fieldset << (
                    input ! [htmlAttr "type" << "hidden", htmlAttr "name" << "referer", htmlAttr "value" << referer] +++
                    label ! [htmlAttr "for" << "page_name"] << "Short Page Name" +++
                    input ! [htmlAttr "type" << "text", htmlAttr "name" << "page_name", htmlAttr "value" << ""] +++
                    thespan ! [htmlAttr "class" << "inline-help"] << "(One Word)" +++
                    d "" "button-group" (
                        input ! [htmlAttr "id" << (cancelLabel ++ "-btn"), htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                        input ! [htmlAttr "id" << (saveLabel ++ "-btn"), htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << saveLabel])))
             (listPagesNav story)

viewPage :: Story -> Page -> String
viewPage story page = commonViewPage story page noHtml

viewStartPage :: Story -> Page -> String
viewStartPage story page = commonViewPage story page (p << (authors story))

commonViewPage :: Story -> Page -> Html -> String
commonViewPage story page preAmble =
    template (Model.title story)
        -- TODO
        -- 1) auto format paragraphs
        -- 2) detect p tags and then disable auto formatting paragraphs
        -- 3) XSS
        ((h1 << (anchor ! [htmlAttr "href" << ("/page/view/" ++ (story_id story) ++ "/start")] << Model.title story)) +++
         preAmble +++
         (thediv << primHtml (prose page)))
        (thediv <<
            toolbox story page)

toolbox :: Story -> Page -> Html
toolbox story page = h3 << "Author's Toolbox" +++ ulist << 
              ((li <<
                (anchor ! [htmlAttr "href" << ("/page/edit/" ++ story_fk_id page ++ "/" ++ page_id page)] << "Edit This Page")) +++
              (editStoryLink (page_id page) (story_fk_id page)) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/page/create/" ++ story_fk_id page)] << "Add a Page")) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/pages/" ++ story_fk_id page)] << "List All Pages")))

editStoryLink "start" story_id = (li << (anchor ! [htmlAttr "href" << ("/story/edit/" ++ story_id)] << "Edit Story Details"))
editStoryLink _ _ = noHtml

--toolboxWrapper
toolboxWrapper c = h3 << "Author's Toolbox" +++ c

toolboxLight :: String -> Html
toolboxLight story = toolboxWrapper ( ulist <<
--toolboxLight story = h3 << "Author's Toolbox" +++ ulist << 
              ((li <<
                (anchor ! [htmlAttr "href" << ("/page/create/" ++ story)] << "Add a Page"))))

-- TODO mark the contents with some special code and then only unescape these magical blocks (???)
-- Consider uring HStringTemplate instead of Text.Xhtml
allowHtml h = h

editPage :: Page -> String
editPage page = 
    template ((story_fk_id page) ++ " " ++ (page_id page)) ((h1 << (story_fk_id page)) +++
        p << ("URL: http://cyoa.haskwhal.com/page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page)) +++
        (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/page/edit/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] <<
            fieldset << (
                textarea ! [htmlAttr "id" << "editor",htmlAttr "name" << "prose", htmlAttr "cols" << "80", htmlAttr "rows" << "30"] << (prose page) +++
                d "" "button-group" (
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << saveLabel]))))
        (toolboxWrapper (ulist << (li << (anchor ! [htmlAttr "href" << ("/page/delete/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] << "Delete This Page"))))
        
viewDeleteForm :: Page -> String
viewDeleteForm page =
    template "Please Confirm: Delete This Page?"
        ((h1 << "Please Confirm: Delete This Page?") +++
         p << ("Are you sure you want to delete Relative URL: /page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page) ++ " this cannot be undone. ") +++
         (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/page/delete/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] <<
            fieldset << (
                input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << deleteLabel])))
        (listPagesNav $ story_fk_id page)
        
listPagesNav story = (toolboxWrapper (ulist << (li <<
                (anchor ! [htmlAttr "href" << ("/pages/" ++ story)] << "List All Pages"))))                
listPages story_id pages =
    template ("All pages in " ++ story_id)
             ((h1 << ("All pages in " ++ story_id)) +++
              (ulist <<
                ((foldl (+++) noHtml $ map pageLi pages) +++
                 (li ! [htmlAttr "id" << "all-pages-add-link"] <<
                   ("Or " +++
                   (anchor ! [htmlAttr "href" << ("/page/create/" ++ story_id)] << "Add a Page"))))))
             $ toolboxLight story_id
    where
        pageLi :: Page -> Html
        pageLi page = li <<
                        (anchor ! [htmlAttr "href" << (link $ page_id page)] <<
                            (link $ page_id page))
        link :: String -> String
        link p = "/page/view/" ++ story_id ++ "/" ++ p

template :: String -> Html -> Html -> String
template title content sidebar = showHtml $
    
        ((header <<
            ((thetitle << title) +++
            (thelink ! [htmlAttr "type" << "text/css", htmlAttr "href" << "http://cyoa.ubuntu/stylo.css",
                        htmlAttr "media" << "screen", htmlAttr "rel" << "stylesheet"] << ""))) +++
         (body << (
             d "header" "" "" +++
             d "center" "" ((d "right-nav" "" sidebar) +++ (d "content" "" content)) +++
             -- (thediv ! [htmlAttr "class" << "content"] << content) +++
             d "footer" "" noHtml) +++
            script ! [htmlAttr "type" << "text/javascript", htmlAttr "src" << "/jquery-1.3.2.min.js"] << "" +++
            script ! [htmlAttr "type" << "text/javascript", htmlAttr "src" << "/behavior.js"] << ""))

d id c stuff | id == ""= thediv ! [htmlAttr "class" << c] << stuff
d id c stuff = thediv ! [htmlAttr "id" << id, htmlAttr "class" << c] << stuff

editStory story =
    template ("Editing \"" ++ (Model.title story) ++ "\" Details")
        ((h1 << ("Editing " ++ (Model.title story) ++ " Details")) +++        
         (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/story/edit/" ++ (story_id story))] <<
            fieldset << (
                input ! [htmlAttr "id" << "story-title", htmlAttr "name" << "title", htmlAttr "type" << "text", htmlAttr "value"  << (Model.title story)] +++
                br +++
                textarea ! [htmlAttr "id" << "story-authors", htmlAttr "name" << "authors", htmlAttr "cols" << "40", htmlAttr "rows" << "8"] << (authors story) +++
                d "" "button-group" (
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++                    
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << saveLabel]))))
        noHtml
        