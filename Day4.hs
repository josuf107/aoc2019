import qualified Data.Map.Strict as Map

main = do
    print day4

day4 = run "193651" "649729"

run lower higher = length $ findNumber (parse lower) (parse higher)

parse = fmap (read . pure)

findNumber lower higher =
    [[a, b, c, d, e, f]
        | a <- [0..9]
        , b <- [0..9]
        , c <- [0..9]
        , d <- [0..9]
        , e <- [0..9]
        , f <- [0..9]
        , and [a <= b, b <= c, c <= d, d <= e, e <= f]
        , checkDouble [a, b, c, d, e, f]
        , let t = [a, b, c, d, e, f] in t > lower && t < higher
    ]

checkDouble :: [Int] -> Bool
checkDouble = any (==2) . Map.fromListWith (+) . fmap (\n -> (n, 1))
