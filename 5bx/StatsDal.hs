module StatsDal where

import Database.HDBC (fromSql, SqlValue)

import Stats

fromResults :: [SqlValue] -> Stats
fromResults [i, w, st, si, b, p, c] = Stats id weight stretches situps backexts pressups chart
      where f :: SqlValue -> Integer
            f x = read $ fromSql x
            id = f i
            weight = f w
            stretches = f st
            situps = f si
            backexts = f b
            pressups = f p
            chart = f c