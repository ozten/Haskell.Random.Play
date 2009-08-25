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

home :: (Monad m) => m Response
home = return $ toResponse "Howdy"