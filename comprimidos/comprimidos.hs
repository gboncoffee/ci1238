import           Control.Monad (replicateM)
import           Data.List     (intercalate, transpose)

indices :: [([Int], Int)] -> Int -> Int -> Int -> String -> String
indices rx x p c acc =
  if x >= p then
    this ++ ";\n"
  else
    indices (tail rx) (x + 1) p c $ this ++ " + "
  where
    this = acc ++ (show ip) ++ " x" ++ (show x)
    (rxp, ip) = head rx

objFunc :: [([Int], Int)] -> Int -> Int -> String
objFunc rx p c = "min: " ++ (indices rx 1 p c "")

data Side = Ge | Le
componentLine :: [Int] -> Int -> Side -> String
componentLine rx qi side =
  (intercalate " + " $ (filter (\l -> not $ null l) $ map (\(j, a) ->
                              if a == 0 then
                                ""
                              else
                                (show a) ++ " x" ++ (show j)
                           ) $ zip [1..] rx)) ++ (case side of
                                                    Ge -> " >= "
                                                    Le -> " <= "
                                                 ) ++
  (show qi) ++ ";\n"

dailyComponent :: [Int] -> [([Int], Int)] -> String
dailyComponent q rx = concat $ map
  (\(ci, qi) -> componentLine ci qi Ge) $ zip columns q
  where
    columns = transpose $ map (\(a, _) -> a) rx

select :: [Int] -> [a] -> [a]
select is xs =
  [ x | (x, i) <- (zip xs [1..]), i `elem` is ]

limitComponent :: [([Int], Int)] -> [(Int, Int)] -> String
limitComponent rx kfl = concat $ map
  (\(ci, qi) -> componentLine ci qi Le) $ zip columns limits
  where
    indexes = map (\(i, _) -> i) kfl
    limits = map (\(_, l) -> l) kfl
    columns = select indexes $ transpose $ map (\(a, _) -> a) rx

nonNegative :: Int -> String
nonNegative p = concat $ map (\i -> "x" ++ (show i) ++ " >= 0;\n") [1..p]

decls :: Int -> String
decls p = "int " ++ (concat $ map (\i -> "x" ++ (show i) ++
                                    if i == p then
                                      ";\n"
                                    else
                                      ", "
                                  ) [1..p])

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs _        = []

main :: IO ()
main = do
  [c, p] <- fmap (map read . words) getLine :: IO [Int]
  q <- fmap (map read . words) getLine :: IO [Int]
  rx <- fmap (map $ (\l -> (init l, last l)) . (map read . words)) $ replicateM p getLine :: IO [([Int], Int)]
  k <- fmap read getLine :: IO Int
  kfl <- fmap (pairs . concat . (map $ map read . words)) $ replicateM k getLine :: IO [(Int, Int)]

  putStr $ objFunc rx p c
  putStr $ (dailyComponent q rx)
  putStr $ (limitComponent rx kfl)
  putStrLn $ (nonNegative p)
  putStrLn $ (decls p)
