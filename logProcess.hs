-- 94.64.110.172 - - [22/Apr/2009:00:57:17 -0700] "GET /users/whoami?cache_bust=Wed%20Apr%2022%202009%2010%3A56%3A56%20GMT+0300%20%28GTB%20Daylight%20Time%29&_=1240387016651 HTTP/1.1" 200 651 "-" "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.0.8) Gecko/2009032609 Firefox/3.0.8 (.NET CLR 3.5.30729)"
-- log processing library
-- module LogProcess (main, parseLog, parseLogLines, parseIP, parseDate, parseRequest, parseResponse, parseUserAgent, uniqr, uniql, uniqify) where

import Data.List
import System.Environment (getArgs)
import System.Time
import Text.Regex.Posix
import Debug.Trace

test = "94.64.110.172 - - [22/Apr/2009:00:57:17 -0700] \"GET /users/whoami?cache_bust=Wed%20Apr%2022%202009%2010%3A56%3A56%20GMT+0300%20%28GTB%20Daylight%20Time%29&_=1240387016651 HTTP/1.1\" 200 651 \"-\" \"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.0.8) Gecko/2009032609 Firefox/3.0.8 (.NET CLR 3.5.30729)\""

data IPAddress = IPAddress [Char] deriving (Show, Eq)

parseIP :: String -> (IPAddress, [Char])

parseIP s = (IPAddress $ snd $ _parseIP s []
                       , fst $ _parseIP s [])
_parseIP :: [Char] -> [Char] -> ([Char], [Char])
_parseIP [] accum = ([], accum)
_parseIP x accum | elem (head x) "1234567890." = _parseIP (tail x) (accum ++ [(head x)])
_parseIP x accum = (x, accum)

ignoreWhitespace :: [Char] -> [Char]
ignoreWhitespace [] = []
ignoreWhitespace x | elem (head x) " \t" = ignoreWhitespace $ tail x
ignoreWhitespace x = x

ignoreDash :: [Char] -> [Char]
ignoreDash [] = []
ignoreDash x | elem (head x) "-" = ignoreDash $ tail x
ignoreDash x = x

-- Would be faster if it used ByteStrings or whatever accroding to docs
parseDate :: String -> (CalendarTime, [Char])
parseDate s = let rs = (s =~ "^\\[([0-9]+)/([a-zA-Z]+)/([0-9]+):([0-9]+):([0-9]+):([0-9]+) ([-+][0-9]*)\\](.*)$" :: (String,String,String,[String]))
                  matches = getMatches rs
              in if length matches > 0
                 then 
                      mktime rs                 
                 else error "Unable to parse a Date" :: (CalendarTime, [Char])
              where getMatches :: (String, String, String, [String]) -> [String]                    
                    getMatches (_, _, _, m) = m :: [String]
                    mktime :: (String, String, String, [String]) -> (CalendarTime, [Char])
                    mktime (_, _, _, [day, month, year, hour, minute, second, offset, stream]) =
                        (CalendarTime ((read year) :: Int)
                                     (logToCtMonth month)
                                     ((read day) :: Int)
                                     ((read hour) :: Int)
                                     ((read minute) ::Int)
                                     ((read second) ::Int)
                                     0 Monday 0 offset 0 False,
                                     stream)
                    logToCtMonth :: String -> Month
                    logToCtMonth m = case m of
                                         "Jan" -> January
                                         "Feb" -> February
                                         "Mar" -> March
                                         "Apr" -> April
                                         "May" -> May
                                         "Jun" -> June
                                         "Jul" -> July
                                         "Aug" -> August
                                         "Sep" -> September
                                         "Oct" -> October
                                         "Nov" -> November
                                         "Dec" -> December
numberOfMatches :: (String, String, String, [String]) -> Int
numberOfMatches (_, _, _, m) = length m

data RequestInfo = RequestInfo {
    method :: [Char]
  , path :: [Char]
  , httpVersion :: [Char]
} deriving (Show, Eq)

-- "GET /users/whoami?cache_bust=Wed%20Apr%2022%20200
parseRequest :: [Char] -> (RequestInfo, [Char])
parseRequest s = let rs = s =~ "^\"([^ ]*) ([^ ]*) ([^\"]*)\"(.*)$" :: (String, String, String, [String])
                     num = numberOfMatches rs
                 in if num > 0
                 then
                    makeRequest rs
                 else
                   error "Unable to parse the Request info out of the log"
                 where makeRequest :: (String, String, String, [String]) -> (RequestInfo, [Char])
                       makeRequest (_, _, _, [method, path, vers, stream]) =
                           ((RequestInfo method path vers), stream)

data ResponseInfo = ResponseInfo {
    code :: Int
  , contentLength :: Int
} deriving (Show, Eq)
parseResponse :: [Char] -> (ResponseInfo, [Char])
parseResponse s = let rs = s =~ "^([0-9]*) ([0-9]*)(.*)$" :: (String, String, String, [String])
                      num = numberOfMatches rs
                  in if num > 0
                  then
                    makeResponse rs
                  else
                    error "Unale to parse the Response info out of the log"
                  where makeResponse :: (String, String, String, [String]) -> (ResponseInfo, [Char])
                        makeResponse (_, _, _, [code, contentLength, stream]) =
                            (ResponseInfo ((read code) :: Int) ((read contentLength) :: Int), stream)
                            
ignoreQuotedDash :: [Char] -> [Char]
ignoreQuotedDash s = let rs = s =~ "^\"-\"(.*)$" :: (String, String, String, [String])
                         num = numberOfMatches rs
                     in if num > 0
                        then
                          stream rs
                        else
                          error "Unable to ignore a quoted dash " ++ s
                     where stream :: (String, String, String, [String]) -> [Char]
                           stream (_, _, _, [st]) = st
                           
parseUserAgent :: [Char] -> ([Char], [Char])
parseUserAgent s =
         let rs = s  =~ "^\"([^\"]*)\"(.*)$" :: (String, String, String, [String])
             num = numberOfMatches rs
         in if num > 0                                         
            then
              userAgent rs
            else
              error "Unable to parse the User Agent"
         where userAgent (_, _, _, [userAgent, stream]) = (userAgent, stream)
         
parseLog :: String -> String
parseLog s = let (ip, stream) = parseIP s
             in
               case ip of
                  IPAddress ip -> ip


parseIPFromLog :: String -> String
parseIPFromLog s = let (ip, stream) = parseIP s
             in
               case ip of
                  IPAddress ip -> ip
                  

-- ignoreWhitespace $ ignoreQuotedDash $ ignoreWhitespace $ snd $ parseResponse $ ignoreWhitespace $ snd $ parseRequest $ ignoreWhitespace $ snd $ parseDate $ ignoreWhitespace $ ignoreDash $ ignoreWhitespace $ ignoreDash $ ignoreWhitespace $ snd $ parseIP test

parseLogLines :: String -> String
-- parseLogLines s = unlines $ foldl' uniql [] $ map parseIPFromLog $ lines s
parseLogLines s = unlines $ foldr uniqr [] $ map parseIPFromLog $ lines s

parsePageSequenceFromLogLines :: String -> String
-- parseLogLines s = unlines $ foldl' uniql [] $ map parseIPFromLog $ lines s
parsePageSequenceFromLogLines s = show $ foldr' associater [] $ map parsePageSequenceFromLog $ lines s
  --where associater_ :: PageSequence -> [PageSequence] -> [String]
    --    associater_ :: s xs = show $ associater_ s xs


-- returns a unique list of strings in the reverse order of when they came in
uniql :: [String] -> String -> [String]
uniql accum s = if s `elem` accum
                then accum
                else s : accum
                
-- returns a unique list of strings in the order the came in
uniqr :: String -> [String] -> [String]
uniqr s accum = if s `elem` accum
                then accum
                else s : accum

uniqify :: [String] -> [String]
--uniqify s = foldr uniqr s []
uniqify s = foldl' uniql s []

{- Working on 3 page problem -}
ip1 = IPAddress "127.0.0.1"
ip2 = IPAddress "65.13.12.111"

t1 = CalendarTime 2009 January 1 6 15 0 0 Monday 0 "+700" 0 False
t2 = CalendarTime 2010 November 1 7 30 0 0 Monday 0 "+700" 0 False

r1 = RequestInfo "GET" "/a" "HTTP 1/1"
r2 = RequestInfo "GET" "/b" "HTTP 1/1"

data Page = Page {
    time :: CalendarTime
  , request :: RequestInfo
} deriving (Show, Eq)

data PageSequence = PageSequence {
    ip :: IPAddress
  , pages :: [Page]
} deriving (Show, Eq)


ps = PageSequence ip1 ([Page t1 r1])
ps1 = PageSequence ip1 ([Page t2 r2])
ps2 = PageSequence ip2 ([Page t2 r2])


parsePageSequenceFromLog :: String -> PageSequence
parsePageSequenceFromLog s =
  let (ip, stream) = ipAndIgnore s  
  in let (t, stream') = parseDate stream
     in pageSeq ip (Page t (request stream'))
  where
      ipAndIgnore :: String -> (IPAddress, String)
      ipAndIgnore s = let (ip, stream) = parseIP s
                      in (ip, ignoreWhitespace $ ignoreDash $  ignoreWhitespace $ ignoreDash $ ignoreWhitespace $ ignoreDash stream)
      request :: String -> RequestInfo
      request stream = fst $ parseRequest $ ignoreWhitespace stream
      
      pageSeq :: IPAddress -> Page -> PageSequence
      pageSeq ip page = PageSequence ip [page]

-- take apart the page sequence, ip and pages
-- create
associater :: PageSequence -> [PageSequence] -> [PageSequence]
associater xs [] = [xs]
associater xs accum =
    case (find (ipPresent xs) accum) of
      Nothing -> addPageSeq xs accum
      Just p  -> concatPageSeq_ p [] accum
    where ipPresent :: PageSequence -> PageSequence -> Bool
          ipPresent a b = ip a == ip b
          
          addPageSeq :: PageSequence -> [PageSequence] -> [PageSequence]
          addPageSeq x accum = x : accum
          
          concatPageSeq_ :: PageSequence -> [PageSequence] -> [PageSequence] -> [PageSequence]
          concatPageSeq_ target preAccum postAccum
              | (ip target) == (ip $ head postAccum) = preAccum `union` ((concatPageSeq (head (pages xs)) target) : (tail postAccum))
              | otherwise = concatPageSeq_ target ((head postAccum) : preAccum) (tail postAccum)

concatPageSeq :: Page -> PageSequence -> PageSequence
concatPageSeq page pageSequence = PageSequence (ip pageSequence) ((pages pageSequence) `union` (page : []))

{- end Working on 3 page problem -}
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ function input
  
main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
              [input, output] -> interactWith function input output
              _ -> putStrLn "Error: exactly two arguments input-file output-file expected"
          myFunction =
            --parseLogLines
            parsePageSequenceFromLogLines