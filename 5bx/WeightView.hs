-- Weight View
module WeightView where

import Happstack.Server (fromData, FromData, look, Request, ServerPartT, toResponse, ToMessage)

import Happstack.Server.SimpleHTTP (askRq, getData, rqInputs, ServerMonad)

import Happstack.Server.HTTP.Types (Response, inputValue, Input)

--import Text.Html ((<<), body, p, renderHtml)
--import Text.XHtml.Strict ((<<), body, p, renderHtml)
--import Text.XHtml.Transitional ((<<), (+++), body, form, p, renderHtml)
import Text.XHtml.Transitional

showWeight :: (ServerMonad m) => String -> m Response
showWeight weight = return $ toResponse $ body <<
    p <<  "Weighing In" +++
    (p << "hey") ! [identifier "the-para"] +++
    form ! [method "post", action "/weight", enctype "multipart/form-data"] <<
        fieldset <<
            (input ! [identifier "weight", name "weight", value weight] +++
            (button << "Save") ! [identifier "submit", name "submit", value "Save", thetype "submit"])