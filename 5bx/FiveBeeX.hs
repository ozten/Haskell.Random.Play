module FiveBeeX where
import Happstack.Server (ServerPartT, toResponse, ToMessage)

import Happstack.Server.SimpleHTTP (askRq, ServerMonad)


--import Text.Html ((<<), body, p, renderHtml)
--import Text.XHtml.Strict ((<<), body, p, renderHtml)
--import Text.XHtml.Transitional ((<<), (+++), body, form, p, renderHtml)
import Text.XHtml.Transitional

home :: (Monad m) => m String
home = return "Howdy"

weight :: (ServerMonad m) => m String
weight = return $ renderHtml $ body <<
--weight = return $ body <<
-- Left off here... figure out how to do content-type text/html
    p <<  "Weighing In" +++
    (p << "hey") ! [identifier "the-para"] +++
    form ! [method "post", action "/weight", enctype "multipart/form-data"] <<
        fieldset <<
            input ! [identifier "weight", name "weight", value "275"] +++
            (button << "Save") ! [identifier "submit", name "submit", value "Save", thetype "submit"]
    