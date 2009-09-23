module Auth where

import Network.CGI

import Config (editors)

data EditorInfo = EditorInfo {
    ipaddress :: String,
    isEditor :: Bool
}

auth :: String -> EditorInfo
auth ip = EditorInfo ip $ (length (filter (\i -> i == ip) editors)) == 1

mustBeAnEditor :: (MonadCGI m, MonadIO m) => String -> m CGIResult -> m CGIResult
mustBeAnEditor ip result = _mustBeAnEditor (auth ip) result


_mustBeAnEditor :: (MonadCGI m, MonadIO m) => EditorInfo -> m CGIResult -> m CGIResult
_mustBeAnEditor e result = check
    where check = case (isEditor e) of
                    True -> result
                    False -> output $ "Error: Access Denied to " ++ (ipaddress e)
