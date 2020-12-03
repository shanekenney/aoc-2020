module Solve.Day03 where

input :: String
input = ".#..........#......#..#.....#..\n....#.............#.#....#..#..\n.....##...###....#..#.......#..\n.#....#..#......#........#.....\n.#.........###.#..........##...\n...............##........#.....\n#..#..........#..##..#....#.#..\n....#.##....#..#...#.#....#....\n...###...#............#.#......\n#.........#..#...............#.\n#.#...........#...............#\n..#.#......#..###.#...#..##....\n.....#..#..#..#............#...\n......#.......#.....#....##....\n#......#...#.......#.#.#.......\n...........##.#.............#..\n.#.........#..#.####...........\n..#...........#....##..........\n#...........#.......#..#.#.....\n.....##...#.....#..##..#..#....\n#.#..........................#.\n##.....#..........#.......##..#\n....#..#............#.#.#......\n.......#.......#..#............\n...#.#..........#..#.....#.....\n.....#...##..##.....##........#\n.#.....#........##............#\n..#....#.#...#.....#.##........\n........##.....#......##...##..\n......#..................#.....\n..##......##.....##...##.......\n......#..#...##......##........\n.#..#..#.#.....................\n.#....#.#...#....#.......##...#\n.####.#..##...#.#.#....#...#...\n.#....#.....#...#..#.........##\n...........#.#####.#.#..##..#..\n.#......##...#..###.#.#....#...\n...#.....#........#..###...#...\n.......#................##.#...\n.##...#.#..................#...\n..#........#....#..........#..#\n..#.........#..................\n...#.#..........#.#..##........\n...#.##..........##...........#\n...........#..#........#.......\n.#....#.#...........#....#.##..\n.#...#..#............#....#.#..\n...#..#...#.........####.#.#...\n..#...#...........###..#...##.#\n......##...#.#.#....##....#....\n#..#.#.....##....#.......#...#.\n.#.....#.....#..#..##..........\n................#.#.#...##.....\n.#.....#............#......#...\n...#...#..#.#....######.....#..\n..#..........##......##.....#..\n......#..#.##...#.#............\n....#.......#..#...#..#.#......\n#......##.#..#........#.....#..\n..#.........#..#.........#.....\n..#.........##.......#.#.#..##.\n...#....##.................#.#.\n...#........##.#.......#.##..##\n....#.#...#...#....#...........\n.........#....##........#......\n...#........#..#.......#...#...\n#.......#....#...#...........#.\n.......#......#...##...........\n.#.#......##.#.......#..#...#..\n.#.....##.#...#......#..#......\n........#.............#.#..#..#\n#...........#....#.....#.##.#.#\n................#...#........##\n#..#.##..#.....#...##.#........\n#.....#.#..##......#.#..#..###.\n....#...#.....#................\n......#...#..##...........#....\n......#.........##.#...#......#\n#...#.#.....#..#.#..#..#......#\n...#.#..#..#.#........###.#....\n..#...#.......#.#.......#......\n...#....#.....#.......#......#.\n#...........#....#..#..#.......\n..........##......##.........##\n##............#..#.#...#..#.#..\n..#.##....##...##..#...#.......\n............##.##..###..#..#...\n......#....##...##.........#...\n......#..#.#......####..#......\n..............#....#..#..##....\n...#.#..#...##.#.......#.#.....\n...#.#....#.......#..#..#..##..\n..........#.........#..........\n...#.....#............#.....##.\n....#.#......................#.\n.........#...#.#...#...........\n...#........#..##.....#...#.#..\n......##.....#.#..#...###.#...#\n#....#..#.#.....#...#..........\n.#.##.###.........#..##.#....#.\n#.........#....#........#...#..\n...........#...............#..#\n###....................#....#..\n.................#....#.....#..\n..........#.........#.......#..\n........#..#....#.....##.......\n#...##.#...#.#.#............#..\n....#.........##.#.#..#...###..\n.##..............#...#.....##.#\n###...#..................#...#.\n.....#..#...#..#...#...........\n.#.................#...#..#..#.\n.#.........###...#.##......###.\n.####............#......#..#...\n....#........#..#.#....#..##..#\n..#....#.#...#.#.....##....#...\n..###..#..#....##....#..#..#...\n...#.#.....#.#....#.....#......\n.....#..........#.#............\n.......#...........#.#..#..#...\n......##........#.....#.......#\n..#.#.....##............#..##..\n....#.#........#...........##..\n#......#..##........#.....#....\n#...#...###..............##....\n#..#........#........#.....##.#\n......##.####........#..#....#.\n...##..#.##.....#...#...#..#...\n#..............###.##..##......\n......................#.....#..\n.........#.#.......#...##.#....\n....#......#..........###..#...\n#...####.#.................#..#\n##.#....#....#.....##..#....#.#\n..#.....#..##.........#.#..#.#.\n.....#.....#...................\n#....##.#.........###....#.....\n#........#.#.......#.#.........\n.##.#...#.....#...#.......##.##\n#..#.............#.............\n..........#.........####.......\n..##..............#..#.#.......\n..#.#.....#........#......##...\n#.#.......#.#................#.\n.#...#........#....##....#.##..\n.#..#...#...#......#.#.........\n......##............#.........#\n.#....#.#.#.........#..#..##...\n#....#......#.......###........\n.......#........##..#...#..###.\n#.##..........#..###..#..#.#...\n.#..#....#..........#.#.##.....\n#..#...#.#...#..#..#.#...#.....\n.........#...#.#............#..\n#..#.............#......##.##..\n...##.......#..................\n....#......#...#.....#......#..\n.....##..#......#....#....#....\n....#...#...#...#.....#........\n.#....#........##....#..#.#...#\n#.......#..#......#......#...#.\n..............#......#......#..\n#......#..##...#........#....#.\n#..#..#..#.....#..#........#...\n#...#.....#...#..........#...##\n........#.......#...#.....#.#..\n...................##.......#..\n.#......#........#.##..#....#..\n.....#.....#...#..#..#......#..\n........##.#..##.........#....#\n.........#.......#.............\n............#.###.###..#.#.....\n.............#....#...........#\n..#.....#.#..##.##........#....\n...#....#....#.........#.....#.\n.#............#......#.........\n..#.#..........##.##......#.#..\n....#.........................#\n..........##...................\n#.......#.#..............#...#.\n...##..#..##...##.#..#.#.#.....\n...########.#..##....#.........\n##.#........##.....#........#..\n#.#.....#........#..#....#...#.\n..#............#.......###.##.#\n#.#............................\n...#.#.#....#..........#..#....\n..###.#.....#.#..#.............\n#........#..........#.#..#.....\n...........#..#....#.........#.\n..#............#.....#.#.......\n#.#............#..#.....#.#.#..\n...#...#.......................\n.#.#.#...##.............#..#..#\n..#.........#..#.....##....##..\n.#...#............#.......#..##\n....#..#.#.#...####............\n#.......#....#..##....##....#..\n.....##.#....#.#..#.......#....\n...........#.......#....##.#.##\n..........#...#....##...#.#....\n..#.............#.............#\n....#..#.....#....#.#..###.#...\n.......#.##.#......#...##...#.#\n.#..#.#..#.#.......#....###.#..\n#..........##...##.........##..\n##..#......##.#.####.#.....#...\n....#.#...#........#..##..#.#..\n.#.............................\n.##..#.#...##.....#....#.....#.\n..##.........#......#.........#\n.#.#........#...#.#.#....##....\n.#.................##.........#\n...#...............#....#......\n..#...#..#..........###..#...##\n..........#..#..........##..#..\n...#.............#.##.#...#....\n...#...........#...............\n......#.........##.#...#...#...\n...#.#........#..#.....#..#...#\n#.#...#....##...#.....#....#...\n#.#.#..#.....#.........#.......\n##...........#..####...........\n#..........#........###...#..#.\n#..#.......#....#......###.....\n..#.....#......#.###......##...\n...#.##..#............#...#....\n.##........#.....#.............\n#....#.##..#...........##.#.#..\n..#.....#.#....#.......#......#\n#..#.......#............#......\n#.......##....#...#..#.........\n.................#..##.........\n..............#..#..#.##.......\n#.#.......................#..#.\n..#..##...........#....#..#..#.\n...#....#.......#.......#....#.\n.....#.#..#.#.....#.........#.#\n..#.#.........#.....#..........\n...#.#.#.......#.#.......#.#..#\n...##...#.#.#.....#.....##....#\n##.......#.#.#.#.......#...##..\n....#.#...........#......#.....\n.#.....#........####...........\n#......#........#.....#..#..#..\n..#..#......#...##.......#....#\n#........#..........#.....#.#..\n.#...........#.....#.....#.....\n..........#..#...#....#....##..\n.....#.#..........#.....##..#..\n......#.........##.............\n..#..#.....##......##........#.\n.#.#.#.#..#.#..#.......#.......\n#.#...####.#.#....#.#........#.\n....#...#.....#......#..##.....\n##.........#.........#..#.#..#.\n..#.#........#.#........#.##...\n#....#......#...#....#.........\n.##.............###....###.#...\n..##.#.......#...#..#......#...\n.....#.##..................#...\n.....#.#...#..#................\n........#..#..#...........#.#.#\n....#.###.....#..#.#.....##..##\n....##.#.........#..##.........\n.##........#......#..###..#.##.\n.........##...............#.##.\n..#...............#.#...#..#.#.\n....#....##.....#...#..#.....#.\n#...#.....................#....\n.....#.#............#...##.#.#.\n...#......#.......#........##.#\n.#.#..#.#....#.##.......##....#\n.........#...#..##.........#...\n.#...#..#....................#.\n.......#...#........#.#..#.#.##\n.#.............#......#..#.#...\n............##.........#....#.#\n#.........##..##...............\n.#.#....#.#..#..........##.....\n..###...#..#.#.......#..#...##.\n.....#....#.#............##.#..\n##.....#.#..#..#...............\n...##...#......#....#..#..#....\n.............#....#..#..##...##\n#.......#............#....##..#\n..#.##.....#.......#....#....#.\n..........#...#.............###\n..#....#.#..................#..\n#.#...#..#...........#.........\n....##..#..##..#..........#....\n#...#...#.#....#.##...#.......#\n#......##.#...##..#.....#......\n....#.......#.#............#...\n#....#...........###...........\n#..#...#...#......#.#..#.......\n...............................\n#........##.............#.#....\n.............#........#....#.##\n........##.####.....##..#......\n#.#.#.#.......##....##.....#...\n.......#..##..#...#............\n..........#...#....#..#.#.#.##.\n...#........##....#...#........\n#..#.##....#....#........#.....\n.##...#.....##...#.............\n.#...#..#.#.....#.##.....#.....\n...........#.............#...#.\n.#..#................#...#..#..\n#..........#......##..##....#..\n####..#...........#.#....#.....\n..#.#.##..#...##........#....##\n.#.......##........#.....#.....\n............#................#.\n.#...#...#.....#.#....#.##..#..\n..#.............#.#....#.#.....\n..............#...........#....\n..............#........#....#..\n..........##........#..#...#...\n...#.#....#.#....#..#.....#...#\n..#......#...........#..#..#.#.\n.....##.....#.####....#........"

data GridCell
  = Tree
  | Empty
  deriving (Show)

isTree :: GridCell -> Bool
isTree Tree = True
isTree Empty = False

toGridCell :: Char -> GridCell
toGridCell '#' = Tree
toGridCell _ = Empty

newtype Grid = Grid [[GridCell]]
  deriving (Show)

readGrid :: String -> Grid
readGrid =
  Grid . fmap (fmap toGridCell) . lines

rows :: Grid -> Int
rows (Grid cells) = length cells

columns :: Grid -> Int
columns (Grid cells) = length $ head cells

getCell :: Grid -> (Int, Int) -> GridCell
getCell grid (row, col) =
  if row < rows grid
    then (cells !! row) !! col'
    else Empty
  where
    (Grid cells) = grid
    col' = col `mod` columns grid

partOne :: String -> Int
partOne input =
  length $ filter isTree $ fmap (getCell grid) path
  where
    grid = readGrid input
    path =
      [ (row, col)
        | n <- [0 .. rows grid],
          let row = n + 1,
          let col = n * 3 + 3,
          row < rows grid
      ]

partTwo :: String -> Int
partTwo input =
  product $ fmap (length . filter isTree . fmap (getCell grid)) paths
  where
    paths = getPath <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    grid = readGrid input
    getPath (right, down) =
      [ (row, col)
        | n <- [0 .. rows grid],
          let row = n * down + down,
          let col = n * right + right,
          row < rows grid
      ]
