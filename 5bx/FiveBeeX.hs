module FiveBeeX where
import Happstack.Server (ServerPartT, toResponse, ToMessage)
import Happstack.Server.HTTP.Types (Response)
import Happstack.Server.SimpleHTTP (askRq, ServerMonad)


--import Text.Html ((<<), body, p, renderHtml)
--import Text.XHtml.Strict ((<<), body, p, renderHtml)
--import Text.XHtml.Transitional ((<<), (+++), body, form, p, renderHtml)
import Text.XHtml.Transitional

home :: (Monad m) => m Response
home = return $ toResponse "Howdy"

weight :: (ServerMonad m) => m Response
weight = return $ toResponse $ body <<
    p <<  "Weighing In" +++
    (p << "hey") ! [identifier "the-para"] +++
    form ! [method "post", action "/weight/t", enctype "multipart/form-data"] <<
        fieldset <<
            (input ! [identifier "weight", name "weight", value "275"] +++
            (button << "Save") ! [identifier "submit", name "submit", value "Save", thetype "submit"])

record_weight :: (ServerMonad m) => m Response
record_weight = return $ toResponse "oh ya"