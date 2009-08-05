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
weight = return (renderHtml (body <<
    p <<  "Weighing In" +++
    (p << "hey") ! [identifier "the-para"]))