module FiveBeeX where

import Control.Monad (mplus, mzero)
import Control.Monad.Reader (liftM)

import qualified Data.ByteString.Char8 as B

import Data.ByteString.Lazy (ByteString, unpack)
import Data.Word (Word8)
import Debug.Trace (trace)

import Happstack.Server (fromData, FromData, look, Request, ServerPartT, toResponse, ToMessage)

import Happstack.Server.HTTP.Types (Response, inputValue, Input)
import Happstack.Server.SimpleHTTP (askRq, getData, rqInputs, ServerMonad)

import Config

--import Text.Html ((<<), body, p, renderHtml)
--import Text.XHtml.Strict ((<<), body, p, renderHtml)
--import Text.XHtml.Transitional ((<<), (+++), body, form, p, renderHtml)
import Text.XHtml.Transitional

home :: (Monad m) => m Response
home = return $ toResponse "Howdy"

showWeight :: (ServerMonad m) => m Response
showWeight = return $ toResponse $ body <<
    p <<  "Weighing In" +++
    (p << "hey") ! [identifier "the-para"] +++
    form ! [method "post", action "/experiment", enctype "multipart/form-data"] <<
        fieldset <<
            (input ! [identifier "weight", name "weight", value "275"] +++
            (button << "Save") ! [identifier "submit", name "submit", value "Save", thetype "submit"])



-- Left off at http://tutorial.happstack.com/tutorial/get-and-post
record_weight :: (ServerMonad m) => Request -> m Response
record_weight req = let weight = getWeight (rqInputs req)
                    in trace (show weight) $
                       return $ toResponse "oh ya"
                    where getWeight :: [(String, Input)] -> String
                          getWeight d = show (inputValue $ snd $ head $ filter (\i -> (fst i == "weight")) d)
                          {-getWeight d = do
                              maybeTheW <- getData'
                              case maybeTheW of
                                  Nothing -> "Error"
                                  Just aWeight -> theWeight aWeight-}
                          
{- http://totherme.livejournal.com/3674.html
It's all about the function types
Instead of focusing on runtime-time imperative behavior
what are the types of the functions you'll need?

Focus on data types and function type signatures

write your qucikCheck properties in parrallel

Stub stuff out

Once you have no stubs left, run quick check
-}