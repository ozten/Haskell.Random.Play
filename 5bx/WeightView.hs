{-
  The View for Weight Resources
-}
module WeightView where

--import Text.Html ((<<), body, p, renderHtml)
--import Text.XHtml.Strict ((<<), body, p, renderHtml)
--import Text.XHtml.Transitional ((<<), (+++), body, form, p, renderHtml)
import Text.XHtml.Transitional

currentWeightView :: String -> String
currentWeightView weight = prettyHtml $  body << (p <<  "Weighing In" +++
    (p << "hey") ! [identifier "the-para"] +++
    form ! [method "post", action "/5bx/weight/current", enctype "multipart/form-data"] <<
        fieldset <<
            (input ! [identifier "weight", name "weight", value weight] +++
            (button << "Save") ! [identifier "submit", name "submit", value "Save", thetype "submit"]))