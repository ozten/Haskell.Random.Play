module View where

import Text.XHtml.Transitional
import Data.String.Utils (replace)
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

viewPage :: Page -> String
viewPage page =
    template (story_fk_id page ++ " " ++ page_id page) $ h1 << story_fk_id page +++
        -- TODO
        -- 1) auto format paragraphs
        -- 2) detect p tags and then disable auto formatting paragraphs
        -- 3) XSS
        (p << primHtml (prose page)) +++
        (thediv <<
            ulist <<
              ((li <<
                (anchor ! [htmlAttr "href" << ("/page/edit/" ++ story_fk_id page ++ "/" ++ page_id page)] << "Edit This Page")) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/page/create/" ++ story_fk_id page)] << "Add a Page")) +++
              (li <<
                (anchor ! [htmlAttr "href" << ("/pages/" ++ story_fk_id page)] << "List All Pages"))))

-- TODO mark the contents with some special code and then only unescape these magical blocks (???)
-- Consider uring HStringTemplate instead of Text.Xhtml
allowHtml h = h
                   
editPage :: Page -> String
editPage page =
    template ((story_fk_id page) ++ " " ++ (page_id page)) $ (h1 << (story_fk_id page)) +++
        p << ("Relative URL: /page/view/" ++ (story_fk_id page) ++ "/" ++ (page_id page)) +++
        (form ! [htmlAttr "method" << "post", htmlAttr "action" << ("/page/edit/" ++ (story_fk_id page) ++ "/" ++ (page_id page))] <<
            fieldset << (
                textarea ! [htmlAttr "name" << "prose", htmlAttr "cols" << "80", htmlAttr "rows" << "30"] << (prose page) +++
                input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << cancelLabel] +++
                input ! [htmlAttr "type" << "submit", htmlAttr "name" << "action", htmlAttr "value" << saveLabel])) +++
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
    thehtml << ((header << thetitle << title) +++
        (body <<
            content))
            
--unescape :: String -> String
--unescape s = replace "&amp;" "&" $ replace "&quot;" "\"" $ replace "&gt;" ">" $ replace "&lt;" "<" s