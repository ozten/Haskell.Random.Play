module StatsModel where

data Stats = Stats {
    id :: Integer,
    weight :: Integer,
    stretches :: Integer,
    situps :: Integer,
    backexts :: Integer,
    pressups :: Integer,
    chart :: Integer {-
    created ::
    updated ::
    -}
} deriving (Eq, Show)

