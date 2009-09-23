module View where

import Text.XHtml.Transitional
import Data.String.Utils (replace) -- MissingH
import Model

saveLabel = "Save"
cancelLabel = "Cancel"
deleteLabel = "Delete"

createPageForm story =
    template ("Create A New Page for " ++ story) $
             h1 << ("Create A New Page for " ++ story) +++
             form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/page/create/" ++ story)] <<                 
                 fieldset << (                    
                    label << "Short Page Name" +++
                    input ! [htmlAttr "type" << "text", htmlAttr "name" << "page_name"] +++
                    --input ! [htmlAttr "type" << "hidden", htmlAttr "name" << "story_name", htmlAttr "value" << story] +++
                    -- TODO input ! [htmlAttr "type" << "submit", htmlAttr "value" << cancelLabel] +++
                    input ! [htmlAttr "type" << "submit", htmlAttr "value" << saveLabel])

viewPage :: Story -> Page -> String
viewPage story page = commonViewPage story page noHtml

viewStartPage :: Story -> Page -> String
viewStartPage story page = commonViewPage story page (p << (authors story))

commonViewPage :: Story -> Page -> Html -> String
commonViewPage story page preAmble =
    template (Model.title story) $
        -- TODO
        -- 1) auto format paragraphs
        -- 2) detect p tags and then disable auto formatting paragraphs
        -- 3) XSS
        (h1 << Model.title story) +++
        preAmble +++
        (thediv << primHtml (prose page)) +++
        (thediv <<
            toolbox story page)

toolbox :: Story -> Page -> Html
toolbox story page = ulist << 
              ((li <<
                (anchor ! [htmlAttr "href" << ("/page/edit/" ++ story_fk_id page ++ "/" ++ page_id page)] << "Edit This Page")) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/story/edit/" ++ (story_id story))] << "Edit Story")) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/page/create/" ++ story_fk_id page)] << "Add a Page")) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/pages/" ++ story_fk_id page)] << "List All Pages")))

-- TODO mark the contents with some special code and then only unescape these magical blocks (???)
-- Consider uring HStringTemplate instead of Text.Xhtml
allowHtml h = h

editPage :: Page -> String
editPage page = 
    template ((story_fk_id page) ++ " " ++ (page_id page)) $ (h1 << (story_fk_id page)) +++
        p << ("Relative URL: /page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page)) +++
        (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/page/edit/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] <<
            fieldset << (
                textarea ! [htmlAttr "id" << "editor",htmlAttr "name" << "prose", htmlAttr "cols" << "80", htmlAttr "rows" << "30"] << (prose page) +++
                d "" "button-group" (
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << saveLabel]))) +++
        (anchor ! [htmlAttr "href" << ("/page/delete/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] << "Delete This Page")
        
viewDeleteForm :: Page -> String
viewDeleteForm page =
    template "Please Confirm: Delete This Page?" $
        (h1 << "Please Confirm: Delete This Page?") +++
        p << ("Are you sure you want to delete Relative URL: /page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page) ++ " this cannot be undone. ") +++
        (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/page/delete/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] <<
            fieldset << (
                input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << deleteLabel])) 
                
listPages story_id pages =
    template ("All pages in " ++ story_id)
             ((h1 << ("All pages in " ++ story_id)) +++
             (ulist <<
                ((foldl (+++) noHtml $ map pageLi pages) +++
                 (li <<
                   ("Or " +++
                   (anchor ! [htmlAttr "href" << ("/page/create/" ++ story_id)] << "Add a Page"))))))
    where
        pageLi :: Page -> Html
        pageLi page = li <<
                        (anchor ! [htmlAttr "href" << (link $ page_id page)] <<
                            (link $ page_id page))
        link :: String -> String
        link p = "/page/view/" ++ story_id ++ "/" ++ p

template :: String -> Html -> String
template title content = showHtml $
    
        ((header <<
            ((thetitle << title) +++
            (thelink ! [htmlAttr "type" << "text/css", htmlAttr "href" << "http://cyoa.ubuntu/stylo.css",
                        htmlAttr "media" << "screen", htmlAttr "rel" << "stylesheet"] << ""))
            -- 
            -- (style ! [htmlAttr "src" << "/stylo.css"])) +++
            ) +++
         (body << (
             d "header" "" "" +++
             d "center" "" ((d "right-nav" "" "abcdefghijkmnopqrrstuv slfkjsdfj lsdkfjsdkfj lskdfjl sldjf") +++ (d "content" "" content)) +++
             -- (thediv ! [htmlAttr "class" << "content"] << content) +++
             d "footer" "" (thediv << "About")) +++
            script ! [htmlAttr "type" << "text/javascript", htmlAttr "src" << "/behavior.js"] << ""))

d id c stuff | id == ""= thediv ! [htmlAttr "class" << c] << stuff
d id c stuff = thediv ! [htmlAttr "id" << id, htmlAttr "class" << c] << stuff

editStory story =
    template ("Editing " ++ (Model.title story) ++ " Details") $
        (h1 << ("Editing " ++ (Model.title story) ++ " Details")) +++        
        (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/story/edit/" ++ (story_id story))] <<
            fieldset << (
                input ! [htmlAttr "name" << "title", htmlAttr "type" << "text", htmlAttr "value"  << (Model.title story)] +++
                textarea ! [htmlAttr "name" << "authors", htmlAttr "cols" << "40", htmlAttr "rows" << "4"] << (authors story) +++
                d "" "button-group" (
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                    input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << saveLabel])))
        