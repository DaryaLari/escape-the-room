{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module EscapeTheRoom where
    import CodeWorld
    import Data.Text

    data DoorState = Opened | Closed
    data DoorColor = Red | Blue | Green
    data Tile = Wall | Floor | Door DoorColor DoorState | Exit | Button DoorColor
    data Coords = Coords Integer Integer
    
    doorColor :: DoorColor -> Color
    doorColor Main.Red = red
    doorColor Main.Blue = blue
    doorColor Main.Green = green

    drawPlayerAt :: Coords -> Picture
    drawPlayerAt (Coords i j) = translated x y (lettering "\x1F6B6")
        where 
            x = fromIntegral i
            y = fromIntegral j

    tile :: Color -> Picture
    tile c = colored c (solidRectangle 0.9 0.9)

    floorTile :: Picture
    floorTile = tile yellow

    wallTile :: Picture
    wallTile = tile black

    button :: Color -> Picture
    button c = colored c (solidCircle 0.3)

    buttonTile :: Color -> Picture
    buttonTile c = button c <> floorTile

    doorTile :: Color -> DoorState -> Picture
    doorTile c Closed = button c <> wallTile
    doorTile c Opened = square c <> floorTile
        where 
            square color = colored color (thickRectangle 0.2 0.8 0.8)
        

    exitTile :: Picture
    exitTile = tile pink

    -- | Render a single tile given its type
    drawTile :: Tile -> Picture
    drawTile Wall = wallTile
    drawTile Floor = floorTile
    drawTile (Door dc s) = doorTile (doorColor dc) s
    drawTile Exit = exitTile
    drawTile (Button dc) = buttonTile (doorColor dc)

    data Dir = DirUp | DirDown | DirLeft | DirRight | DirAnother

    stringToDir :: String -> Dir
    stringToDir "Up" = DirUp
    stringToDir "Down" = DirDown
    stringToDir "Left" = DirLeft
    stringToDir "Right" = DirRight
    stringToDir _ = DirAnother

    coordsByDir :: Dir -> Coords -> Coords
    coordsByDir DirUp (Coords x y) = Coords x (y + 1)
    coordsByDir DirDown (Coords x y) = Coords x (y - 1)
    coordsByDir DirLeft (Coords x y) = Coords (x - 1) y
    coordsByDir DirRight (Coords x y) = Coords (x + 1) y
    coordsByDir DirAnother (Coords x y) = Coords x y


    gameMap :: [[Tile]]
                -- 0       1      2      3      4      5      6      7      8      9     10      11     12     13    14     15     16     17     18      19     20
    gameMap =   [[Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall ], -- 0
                [Wall , Floor, Wall , Button Main.Blue, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Button Main.Red, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 1
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 2
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 3
                [Wall , Floor, Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Door Main.Blue Closed, Door Main.Green Closed, Floor, Floor, Wall , Floor, Floor, Floor, Wall , Floor, Wall ], -- 4
                [Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall ], -- 5
                [Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall ], -- 6
                [Wall , Floor, Wall , Wall , Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall , Floor, Wall ], -- 7
                [Wall , Floor, Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall ], -- 8
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall , Wall , Wall , Wall , Wall , Wall , Wall , Floor, Wall , Wall , Floor, Wall , Wall ], -- 9
                [Wall , Floor, Floor, Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall , Floor, Floor, Button Main.Green, Wall ], -- 10
                [Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Door Main.Red Closed, Wall , Wall , Wall , Wall , Wall ], -- 11
                [Wall , Floor, Wall , Floor, Wall , Floor, Door Main.Blue Closed, Floor, Wall , Floor, Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall ], -- 12
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall ], -- 13
                [Wall , Floor, Wall , Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall , Floor, Wall , Floor, Wall , Floor, Floor, Floor, Wall ], -- 14
                [Wall , Wall , Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Wall , Wall , Floor, Wall , Floor, Wall , Wall , Wall , Floor, Wall ], -- 15
                [Wall , Floor, Floor, Floor, Wall , Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall , Floor, Wall , Floor, Floor, Floor, Floor, Floor, Wall ], -- 16 
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Wall , Wall , Floor, Wall , Wall , Wall , Wall , Wall , Door Main.Green Closed, Wall ], -- 17
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall , Button Main.Blue, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Exit , Wall ], -- 18
                [Wall , Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Floor, Floor, Wall , Floor, Wall , Wall , Wall , Wall , Wall , Floor, Wall ], -- 19
                [Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall , Wall ]] -- 20

    initialWorld :: Level
    initialWorld = Level (Coords 1 1) tileByCoords

    updateWorld :: Double -> Level -> Level
    updateWorld _ lvl = lvl

    drawWorld:: Level -> Picture
    drawWorld (Level c f) = scaled (0.75) (0.75) (
            translated x y (drawPlayerAt c <> drawLevelMap f))
        where
            sizeX = 21 -- later would be easier to change map size
            sizeY = 21
            x = - sizeX / 2
            y = - sizeY / 2
        
    -- | A level is a tile map together with character's initial location.
    data Level = Level Coords (Coords -> Tile)

    tileByCoords :: Coords -> Tile
    tileByCoords (Coords i j) = gameMap !! (fromIntegral j) !! (fromIntegral i)

    -- | Try move character in a given direction.
    tryMove :: Dir -> Level -> Level
    tryMove dir (Level coords f)
        | canMove (f nextCoords) = Level nextCoords f
        | otherwise = Level coords f
            where nextCoords = coordsByDir dir coords

    canMove :: Tile -> Bool
    canMove Wall = False
    canMove Floor = True
    canMove (Door _ Closed) = False
    canMove (Door _ Opened) = True
    canMove Exit = True
    canMove (Button _) = True

    -- | Draw a given level map.
    drawLevelMap :: (Coords -> Tile) -> Picture
    drawLevelMap tileF = reduce (reduce (mapThroughMap applyF) [] (++)) blank (<>)
        where
            applyF :: Coords -> Picture
            applyF (Coords i j) = translated x y (drawTile (tileF (Coords i j)))
                where
                    x = fromInteger i
                    y = fromInteger j
        

    -- | map function applied to the whole gameMap
    --mapThroughMap :: (Coords -> Smth) -> Smth
    mapThroughMap func = iterateWith (\y -> iterateRow func maxX y) maxY
        where
            maxX = 21 - 1 -- later would be easier to change map size
            maxY = 21 - 1
            --iterateWith :: (Integer -> Smth) -> Integer -> Smth
            iterateWith f j
                | j < 0 = []
                | otherwise = iterateWith f (j - 1) ++ [f j]
            
            -- | iterates through j-row
            --iterateRow :: (Coords -> Smth) -> Integer -> Integer -> Smth
            iterateRow cellF i j = iterateWith mapCell i
                where
                    mapCell x = cellF (Coords x j)
        

    --reduce :: [_] -> _acc -> (_acc -> _ -> _acc) -> _acc
    reduce [] acc _ = acc
    reduce (x:xs) acc f = f (reduce xs acc f) x

    eqCoords :: Coords -> Coords -> Bool
    eqCoords (Coords x1 y1) (Coords x2 y2) = x1 == x2 && y1 == y2
        

    eqDoorColor :: DoorColor -> DoorColor -> Bool
    eqDoorColor Main.Red Main.Red = True
    eqDoorColor Main.Blue Main.Blue = True
    eqDoorColor Main.Green Main.Green = True
    eqDoorColor _c1 _c2 = False

    oneOf :: DoorColor -> [DoorColor] -> Bool
    oneOf c colors = reduce colors False acc
        where
            acc a arrColor = a || eqDoorColor arrColor c

    -- | Open some doors (Doors to open, Original map) -> Map with doors open
    openDoors4 :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
    openDoors4 colors cMap = reduce (reduce (mapThroughMap id) [] (++)) (\c -> cMap c) accF
        where
            accF :: (Coords -> Tile) -> Coords -> (Coords -> Tile)
            accF f c
                | isDoor (cMap c) && oneOf (getDoorColor (cMap c)) colors = getNewMap c
                | otherwise = f
                where
                isDoor :: Tile -> Bool
                isDoor (Door _ _) = True
                isDoor _ = False
        --          getDoorColor :: (Door DoorColor DoorState) -> DoorColor
                getDoorColor (Door col _) = col
                getNewMap :: Coords -> Coords -> Tile
                getNewMap doorCoord checkCoord
                    | eqCoords doorCoord checkCoord = invertState (cMap doorCoord)
                    | otherwise = f checkCoord
                invertState :: Tile -> Tile
                invertState (Door color Opened) = Door color Closed
                invertState (Door color Closed) = Door color Opened
                invertState t = t
            
    handleWorld :: Event -> Level -> Level
    handleWorld (KeyPress key) l = checkF (tryMove (stringToDir (unpack key)) l)
        where
            checkF (Level c f)
                | isButton (f c) = Level c (openDoors4 (getColors (f c)) f)
                | isExit (f c) = Level (Coords 100 100) f
                | otherwise = Level c f
                where
                    isButton :: Tile -> Bool
                    isButton (Button _) = True
                    isButton _ = False
                    isExit :: Tile -> Bool
                    isExit Exit = True
                    isExit _ = False
                    getColors :: Tile -> [DoorColor]
                    getColors (Button c') = [c']
                    getColors _ = []
    handleWorld _ l = l
    
    main :: IO ()
    main = interactionOf initialWorld updateWorld handleWorld drawWorld