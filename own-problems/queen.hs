
import Data.List

-- check do two queens attack each other
doesConflict :: () => (Int, Int) -> (Int, Int) -> Bool
doesConflict (x1, y1) (x2, y2) = (x1 == x2) || (y1 == y2) || (abs(x1-x2) == abs(y1-y2))

-- check do queens conflict with new one
doConflict :: () => [(Int, Int)] -> (Int, Int) -> Bool
doConflict queens candidate = True `elem` (map (doesConflict candidate) queens)

-- check is current placement is good
-- placement where is no queen considered good, cause no one attacks each other
isGood :: () => [(Int, Int)] -> Bool
isGood [] = True
isGood (queen:queens)
  | doConflict queens queen = False
  | otherwise = isGood queens

-- generate all possible queen placement
generatePlacements :: () => Int -> [[(Int, Int)]]
generatePlacements x = (map (zip [1..x]) (permutations [1..x]))

-- take only good placements
filterGoodPlacements :: () => [[(Int, Int)]] -> [[(Int, Int)]]
filterGoodPlacements = filter (isGood)

--
visualize :: () => [[(Int, Int)]]

run n = (filterGoodPlacements . generatePlacements) n

