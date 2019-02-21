-- | Levels for Escape the Room.
module EscapeTheRoom.Levels where

    import           Data.List (elemIndex, findIndex)
    
    -- | Tiles used in Escape the Room game.
    data Tile
      = Wall              -- ^ An unpassable wall.
      | Floor             -- ^ Floor to walk on.
      | Door DoorColor    -- ^ Door of a given color.
      | Button DoorColor  -- ^ A button that opens/toggles
                          -- all doors of the same color.
      | Exit              -- ^ An exit.
    
    -- | Available door and button colors.
    data DoorColor
      -- standard colors
      = CRed | CGreen | CBlue
      -- extra colors, used in some levels
      | CCyan | COrange
      | CPink | CPurple | CMagenta | CViolet
      | CYellow | CYellow2 | CYellow3 | CYellow4
      | CBlack | CBlack2 | CBlack3 | CBlack4
      | CWhite | CGray
    
    -- | Coordinates on a level map.
    data Coords = Coords Int Int
    
    -- | A level map with initial coordinates.
    data Level = Level Coords (Coords -> Tile)
    
    -- | A list of all level maps.
    levels :: [Level]
    levels =
      [ -- level1     -- Author: Kamilla Borodina
        level2       -- Author: Alina Chasova
      , level3       -- Author: Nikolay Gaivoronskiy
      -- , level4       -- Author: Kamill Gusmanov
      , level5       -- Author: Alik Khilazhev
      , level6       -- Author: Darya Larionova
      , level7       -- Author: Elena Lebedeva
      , level8       -- Author: Azat Sultanov
      , level9       -- Author: Ruslan Tushov
      ]
    
    -- | Author: Alina Chasova
    level2 :: Level
    level2 = Level (Coords (-1) 7) myLevel
      where
        myLevel (Coords i j)
          | abs i == 10 || abs j == 10
            || abs j == 8 && abs i >= 0 && abs i <= 3
                        && not (i == 3 && j == 8)
            || abs i == 8 && (abs j < 4 && abs j > 0 || j == 5)
            || abs j == 7 && abs i > 3 && abs i < 6
            || abs i == 7 && abs j <= 5 && abs j >= 4
            || j == -6 && abs i == 6 || j == 9 && abs i < 9 && abs i > 5
            || j == 8 && abs i < 7 && abs i > 4
            || j == 7 && abs i < 10 && abs i > 7
            || j == 6 && abs i < 10 && abs i > 6
            || (j == 4 || j == 0) && abs i > 1 && abs i < 5
            || (j == 3 || j == 1) && abs i > 1 && abs i < 6
            || j == 2 && abs i > 3 && abs i < 6
            || (j == -2 || j == -6) && i > -3 && i < 3 || j == -3 && i > -2 && i < 2
            || j == -4 && i == 0 || j == -5 && abs i == 3 = Wall
          | j == 2 && abs i == 2 || j > 5 && j < 8 && -i == j || j == 7 && i == -6
            || j == 8 && i == 9  = Door CBlack2
          | j == 8 && (i == -8 || i == -7) = Door CBlack
          | j == 8 && (i == 8 || i == 7) = Door CBlack3
          | j > 5 && j < 8 && i == j || j == 7 && i == 6
            || j == 8 && i == -9 = Door CBlack4
          | j == 2 && abs i == 3 || i == 8 && j == 0 = Door CYellow
          | i == -8 && j == 0 = Door CYellow2
          | j == 8 && i == 3  = Door CYellow3
          | j == -9 && i == 0 = Door CYellow4
          | j == -9 && (i == -8 || i == 7) || j == -8 && (i == -9 || i == 8)
            || j == -7 && i == 9 = Door CBlue
          | j == -7 && i == -9 || j == -8 && (i == -8 || i == 9)
            || j == -9 && (i == -7 || i == 8) = Door CRed
          | j == -4 && i == -6 || j == -5 && i == -5
            || j == -6 && i == -4  = Door CPink
          | j == 4 && i == 6 || j == 5 && i == 5
            || j == 6 && i == 4 = Door CPurple
          | j == -9 && i == -9 = Button CBlack2
          | j == -9 && i == 9 = Button CBlack3
          | j == 1 && i == 9 = Button CBlack
          | j == -4 && i == -8 = Button CBlack4
          | j == -5 && i == -6 || j == -6 && i == -5 = Button CRed
          | j == 5 && i == 6 || j == 6 && i == 5 = Button CBlue
          | j == 6 && i == -3 = Button CYellow
          | j == -7 && i == 0 = Button CYellow2
          | j == 9 && i == -4 = Button CYellow4
          | j == 1 && i == -9 || j == 2 && i == 9 = Button CYellow3
          | j == 5 && i == -9 = Button CPurple
          | j == 5 && i == 9  = Button CPink
          | abs j == 9 && abs i == 9 || j == 9 && i == 5 = Exit
          | otherwise = Floor
    
    -- | Author: Nikolay Gaivoronskiy
    level3 :: Level
    level3 = Level (Coords (-5) (-10)) myLevelMap
      where
        myLevelMap (Coords i j)
          | j == -10 && i > -6 && i <= -1 = Floor
          | j == -9 && i > -5 && i <= -1 = Floor
          | j == -8 && i > -4 && i <= -1 = Floor
          | j == -7 && i > -3 && i <= -1 = Floor
    
          | j == -10 && i > -1 && i == 3 = Button CPurple
          | j == -10 && i > -1 && i < 3 = Door CRed
          | j == -9 && i > -1 && i < 3 = Door CRed
          | j == -8 && i > -1 && i < 2 = Door CRed
          | j == -7 && i > -1 && i < 1 = Door CRed
    
          | i == -1 && j >= -10 && j < 6 = Floor
          | j == 4 && i >= -1 && i <= 2 = Floor
          | j == 3 && i >= -1 && i <= 7 = Floor
          | j == 2 && i == 6 = Floor
          | j == 1 && i >= 4 && i <= 8 = Floor
          | j == 0 && (i >= 4 && i <= 5 || i >= 7 && i <= 8) = Floor
          | j == -1  && (i == 4 || i == 8) = Button CRed
          | j == -2 && (i == 4 || i == 8) = Door CPurple
          | j == -3 && (i == 4 || i == 8) = Door CPurple
    
          | j == -3 && (i == 6) = Exit
          | j == -4 && i >= 5 && i <= 7 = Exit
          | j == -4 && (i == 4 || i == 8) = Door CPurple
          | otherwise = Wall
    
    -- | Author: Alik Khilazhev
    level5 :: Level
    level5 = Level (Coords 0 0) getTile
      where
        getTile (Coords i j) = (levelMap !! fromIntegral (j+halfOfMap) ) !! fromIntegral (i+halfOfMap)
        mapSize = 21
        halfOfMap = (fromIntegral mapSize - 1) `div` 2
        levelMap = [
          replicate mapSize Wall, -- first row
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 2
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 3
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 4
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 5
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 6
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 7
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 8
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 9
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 10
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 11
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 12
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 13
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 14
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 15
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 16
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 17
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 18
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 19
          Wall: (take (mapSize - 2) $ repeat Floor ) ++[Wall], -- 20
          replicate mapSize Wall] -- last row
    
    -- | Author: Darya Larionova
    level6 :: Level
    level6 = Level (Coords 1 1) tileByCoords
      where
        tileByCoords (Coords i j) = gameMap !! (fromIntegral j) !! (fromIntegral i)
    
                    -- 0       1      2      3      4      5      6      7      8      9     10      11     12     13    14     15     16     17     18      19     20
        gameMap =   [[Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall ], -- 0
                     [Wall , Floor, Wall , Button CBlue, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Button CRed, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 1
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 2
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 3
                     [Wall , Floor, Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Door CBlue, Door CGreen, Floor, Floor, Wall , Floor, Floor, Floor, Wall , Floor, Wall ], -- 4
                     [Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall ], -- 5
                     [Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall ], -- 6
                     [Wall , Floor, Wall , Wall , Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall , Floor, Wall ], -- 7
                     [Wall , Floor, Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall ], -- 8
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall , Wall , Wall , Wall , Wall , Wall , Wall , Floor, Wall , Wall , Floor, Wall , Wall ], -- 9
                     [Wall , Floor, Floor, Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall , Floor, Floor, Button CGreen, Wall ], -- 10
                     [Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Door CRed, Wall , Wall , Wall , Wall , Wall ], -- 11
                     [Wall , Floor, Wall , Floor, Wall , Floor, Door CBlue, Floor, Wall , Floor, Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall ], -- 12
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall ], -- 13
                     [Wall , Floor, Wall , Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall , Floor, Wall , Floor, Wall , Floor, Floor, Floor, Wall ], -- 14
                     [Wall , Wall , Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall ], -- 15
                     [Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall , Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall ], -- 16
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Wall , Wall , Floor, Wall , Wall , Wall , Wall , Wall , Door CGreen, Wall ], -- 17
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Button CBlue, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Exit , Wall ], -- 18
                     [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Floor, Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 19
                     [Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall ]] -- 20
    
    -- | Author: Elena Lebedeva
    level7 :: Level
    level7 = Level (Coords 1 1) myCoolLevelMap
      where
        myCoolLevelMap (Coords i j)
            | i == 4 && j == 2 = (Button CBlue)
            | i == 3 && j == 5 = (Door CBlue)
            | i == 4 && j == 7 = (Button CRed)
            | i == 3 && j == 10 = (Button CGreen)
            | i == 10 && j == 18 = (Button CMagenta)
            | i == 9 && j == 12 = (Door CMagenta)
            | i == 6 && j == 17 = (Door CCyan)
            | i == 12 && j == 4 = (Door CViolet)
            | i == 19 && j == 19 = Exit
            | (i == 6 && j == 9) || (i == 14 && j == 15) = (Door CRed)
            | (i == 4 && j == 12) || (i == 7 && j == 7) = (Door CGreen)
            | (i == 2 && j == 16) || (i == 9 && j == 9) = (Button CCyan)
            | (i == 8 && j == 15) || (i == 10 && j == 3) = (Button CViolet)
            | (i == 13 && j == 12) || (i == 15 && j == 2) = (Button CWhite)
            | (i == 13 && j == 6) || (i == 18 && j == 15) = (Door CWhite)
            | (i == 2 && j == 18) || (i == 14 && j == 8) = (Button COrange)
            | (i == 15 && (j == 10 || j == 6)) || (i == 19 && j == 6) = (Door COrange)
            | i == 0 || j == 0 || i == 20 || j == 20 = Wall
            | i < 7 && (j == 5 || j == 12) = Wall
            | i == 6 = Wall
            | i > 6 && i < 12 && (j == 7 || j == 12) = Wall
            | i > 11 && (j == 6 || j == 15) = Wall
            | j > 6 && j < 16 && (i == 11 || i == 17) = Wall
            | i > 10 && i < 17 && j == 10 = Wall
            | i == 15 && j > 15 = Wall
            | i == 12 && j < 6 = Wall
            | otherwise = Floor
    
    -- | Author: Azat Sultanov
    level8 :: Level
    level8 = Level (Coords 1 1) levelDescription2
      where
        levelDescription2 (Coords x y)
          | x == 3 && y == -7 = Exit
          | x == 3 && y == 3 = Button CGreen
          | x == 6 && y == 2 = Button CBlue
          | x == 2 && y == 0 = Door CGreen
          | x == 4 && y == 3 = Door CGreen
          | x == 1 && y == -4 = Door CBlue
          | abs x == 10 || abs y == 10 = Wall
          | abs x `mod` 4 == 0 || abs y `mod` 4 == 0 = Wall
          | otherwise = Floor
    
    -- | Author: Ruslan Tushov
    level9 :: Level
    level9 = parseLevel textLevel
      where
        -- constant
        -- edit level here with chars
        --   '#' - wall
        --   '.' - floor
        --   '*' - exit
        --   '@' - player
        --   lowercase - button
        --   uppercase - wall
        -- button and door letters
        --   rRgGbByYpPoO
        textLevel :: [String]
        textLevel = [
          "################################",
          "#.#.....#...#..g#......#...#...#",
          "#.#.###.###.#.####.##.##.#.#.#.#",
          "#.....#.......#.#........#....##",
          "#R#############.#.##.######.#..#",
          "#...............#..........#...#",
          "###.#########.###########.#..#.#",
          "#...#.....#.#.#.....#...#..#...#",
          "#.###.#.#.#.#.#####.#.##..#..@.#",
          "#.#...#.....#.......#....#..#..#",
          "###.#####G#########.#.##..#..#.#",
          "#...#.#b#.#.........#...####..##",
          "###.#...#.#.#####B#.#.#.#.#o#..#",
          "#...#.###.#.#.....#.#.#...#..#.#",
          "#.###...#.#.#.#O#.#.#.##.##.#..#",
          "#.#.###.#.#.#.#*#.#.#..#..#..###",
          "#.#...#.#.#.#.###.#.#####.##...#",
          "#.#.#...#...#.....#r#.....#..#.#",
          "#.#.#############.###.#.#.#Y####",
          "#.#.........#.......#.#.#.#..#.#",
          "#.####.##.#####.###.#...####.#.#",
          "#...#...#.........#...#...#....#",
          "###.#######################.#..#",
          "#.............#y...........#..##",
          "#.#.#####.#.#.###########.#.#..#",
          "#.#.#p..###.#......#..#.....##.#",
          "#.#.###.#.#.##.#.#.#.##.##.#...#",
          "#.#...#...#......#....P#...#.###",
          "#.###.###.#.#.#..###.#...#...#.#",
          "#.#.#.#.#.###..#...#.#.####.##.#",
          "#...#...#......#.#...#.........#",
          "################################"]
    
        -- parsing
        parseLevel :: [String] -> Level
        parseLevel [] = error "empty rows"
        parseLevel rows = Level (Coords playerX playerY) (\(Coords x y) -> tiles !! y !! x)
          where
            sizeX = length $ head rows
 --           sizeY = length rows
    
            playerY = case (findIndex (elem '@') rows) of
              Nothing -> error "player position not specified"
              Just v  -> v
            (Just playerX) = elemIndex '@' $ rows !! playerY
    
            tiles = parseTileRows rows
              where
                parseTileRows:: [String] -> [[Tile]]
                parseTileRows [] = error "impossible"
                parseTileRows (row:rows2) = assert "equal rows length" (length row == sizeX) $
                  (map parseTile $ row) : parseTileRows rows2
    
                parseTile :: Char -> Tile
                parseTile '.' = Floor
                parseTile '@' = Floor -- player
                parseTile '#' = Wall
                parseTile '*' = Exit
                parseTile 'R' = Door CRed
                parseTile 'r' = Button CRed
                parseTile 'B' = Door CBlue
                parseTile 'b' = Button CBlue
                parseTile 'Y' = Door CGray
                parseTile 'y' = Button CGray
                parseTile 'G' = Door CGreen
                parseTile 'g' = Button CGreen
                parseTile 'P' = Door CPurple
                parseTile 'p' = Button CPurple
                parseTile 'O' = Door COrange
                parseTile 'o' = Button COrange
                parseTile _   = error "unknown map character"
    
        -- assert
        assert :: String -> Bool -> a -> a
        assert _ True        = id
        assert message False = error message
