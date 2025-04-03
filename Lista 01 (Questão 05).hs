ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo x y = (x || y) && not (x && y)