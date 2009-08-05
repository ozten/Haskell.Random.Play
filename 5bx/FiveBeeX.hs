module FiveBeeX where
import Happstack.Server (ServerPartT, toResponse, ToMessage)

home :: (Monad m) => m String
home = return "Howdy"