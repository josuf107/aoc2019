import Data.List

main = do
    print (run input)
    print (run2 test1)
    print (run2 test2)
    print (run2 input)

type Point = (Int, Int, Int)
type Vector = (Int, Int, Int)
type Moon = (Point, Vector)

input :: [Point]
input =
    [ (19, -10, 7)
    , (1, 2, -3)
    , (14, -4, 1)
    , (8, 7, -6)
    ]

test2 :: [Point]
test2 =
    [ (-8, -10, 0)
    , (5, 5, 10)
    , (2, -7, 3)
    , (9, -8, -3)
    ]

test1 :: [Point]
test1 =
    [ (-1, 0, 2)
    , (2, -10, -7)
    , (4, -8, 8)
    , (3, 5, -1)
    ]

run = sum . fmap energy . (!!1000) . iterate step . initialize
-- run2 input = (+1) . length . takeWhile (/=(initialize input)) . drop 1 . iterate step . initialize $ input

run2 moons = foldl1 lcm . fmap (calculatePeriod (initialize moons)) $ [\(x, _, _) -> x, \(_, y, _) -> y, \(_, _, z) -> z]

calculatePeriod :: [Moon] -> (Point -> Int) -> Int
calculatePeriod moons axis = 
    let
        sliceAxis (p, v) = (axis p, axis v)
        initialState = fmap sliceAxis moons
        steps = fmap (fmap sliceAxis) . iterate step $ moons
    in (+1) . length . takeWhile (/=initialState) . drop 1 $ steps

energy ((x, y, z), (vx, vy, vz)) = (sum . fmap abs $ [x, y, z]) * (sum . fmap abs $ [vx, vy, vz])

step :: [Moon] -> [Moon]
step moons = fmap move . reverse $ update moons []

move :: Moon -> Moon
move ((x, y, z), v@(vx, vy, vz)) = ((x + vx, y + vy, z + vz), v)

update :: [Moon] -> [Moon] -> [Moon]
update [] moonsBefore = moonsBefore
update (m:moonsAfter) moonsBefore = update moonsAfter (gravitate m (moonsAfter ++ moonsBefore):moonsBefore)

gravitate :: Moon -> [Moon] -> Moon
gravitate = foldl (\moon moonFriend -> gravitateMoon moon moonFriend)

gravitateMoon :: Moon -> Moon -> Moon
gravitateMoon (p@(x1, y1, z1), (vx, vy, vz)) ((x2, y2, z2), _) =
    let
        gravitateAxis p1 p2 = if p1 < p2 then (+1) else if p1 > p2 then (subtract 1) else id
        vx' = gravitateAxis x1 x2 vx
        vy' = gravitateAxis y1 y2 vy
        vz' = gravitateAxis z1 z2 vz
    in (p, (vx', vy', vz'))

initialize :: [Point] -> [Moon]
initialize points = zip points (repeat (0, 0, 0))
