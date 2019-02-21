{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module EscapeTheRoom where
    import CodeWorld
    import Data.Text
    import EscapeTheRoom.Levels

    data DoorState = Opened | Closed
 --   data DoorColor = Red | Blue | Green
 --   data Tile = Wall | Floor | Door DoorColor DoorState | Exit | Button DoorColor
--    data Coords = Coords Integer Integer
    
    doorColor :: DoorColor -> Color
    doorColor CRed = red
    doorColor CBlue = blue
    doorColor CGreen = green
    doorColor _ = white

    drawPlayerAt :: Coords -> Picture
    drawPlayerAt (Coords i j) = translated x y (lettering ":D")
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

    doorTile :: Color -> Picture
    doorTile c = button c <> wallTile
--    doorTile c = square c <> floorTile
--        where 
--            square color = colored color (thickRectangle 0.2 0.8 0.8)
        

    exitTile :: Picture
    exitTile = tile pink

    -- | Render a single tile given its type
    drawTile :: Tile -> Picture
    drawTile Wall = wallTile
    drawTile Floor = floorTile
    drawTile (Door dc) = doorTile (doorColor dc)
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
        
    -- | A level is a tile map together with character's initial location.
--    data Level = Level Coords (Coords -> Tile)

--    tileByCoords :: Coords -> Tile
--    tileByCoords (Coords i j) = gameMap !! (fromIntegral j) !! (fromIntegral i)

    -- | Try move character in a given direction.
    tryMove :: Dir -> Level -> Level
    tryMove dir (Level coords f)
        | canMove (f nextCoords) = Level nextCoords f
        | otherwise = Level coords f
            where nextCoords = coordsByDir dir coords

    canMove :: Tile -> Bool
    canMove Wall = False
    canMove Floor = True
    canMove (Door _) = False
    canMove Exit = True
    canMove (Button _) = True

    -- | Draw a given level map.
    drawLevelMap :: (Coords -> Tile) -> Picture
    drawLevelMap tileF = reduce (reduce (mapThroughMap applyF) [] (++)) blank (<>)
        where
            applyF :: Coords -> Picture
            applyF (Coords i j) = translated x y (drawTile (tileF (Coords i j)))
                where
                    x = fromIntegral i
                    y = fromIntegral j
        

    -- | map function applied to the whole gameMap
    mapThroughMap :: (Coords -> a) -> [[a]]
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
        

    reduce :: [a] -> _acc -> (_acc -> a -> _acc) -> _acc
    reduce [] acc _ = acc
    reduce (x:xs) acc f = f (reduce xs acc f) x

    eqCoords :: Coords -> Coords -> Bool
    eqCoords (Coords x1 y1) (Coords x2 y2) = x1 == x2 && y1 == y2
        

    eqDoorColor :: DoorColor -> DoorColor -> Bool
    eqDoorColor CRed CRed = True
    eqDoorColor CBlue CBlue = True
    eqDoorColor CGreen CGreen = True
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
                isDoor (Door _) = True
                isDoor _ = False
        --          getDoorColor :: (Door DoorColor DoorState) -> DoorColor
                getDoorColor (Door col) = col
                getDoorColor _ = CRed
                getNewMap :: Coords -> Coords -> Tile
                getNewMap doorCoord checkCoord
                    | eqCoords doorCoord checkCoord = Floor
                    | otherwise = f checkCoord

    data Status = Prepare | Play | Win

    data State = State [Level] Status [(Coords, DoorColor)]

    isLevelComplete :: State -> Bool
    isLevelComplete (State (l:_) _ _) 
        | isExit (getTile l) = True
        | otherwise = False
            where
                getTile (Level c f) = f c
                isExit :: Tile -> Bool
                isExit Exit = True
                isExit _ = False
    isLevelComplete _ = True           
                
    solution5 :: IO ()
    solution5 = interactionOf initialWorld5 updateWorld5 handleWorld5 drawWorld5
        where
            initialWorld5 :: State
            initialWorld5 = State levels Prepare []
            updateWorld5 :: Double -> State -> State
            updateWorld5 _ w = w
            handleWorld5 :: Event -> State -> State
            handleWorld5 (KeyPress _) (State ls Prepare ds) = State ls Play ds
            handleWorld5 (KeyPress _) (State (l:[]) Win ds) = State [l] Win ds
            handleWorld5 (KeyPress _) (State (_:ls) Win ds) = State ls Prepare ds
            handleWorld5 (KeyPress key) (State (l:ls) Play ds) = newS
                where
                    isExit :: Tile -> Bool
                    isExit Exit = True
                    isExit _ = False
                    checkF (Level c f)
                        | isButton (f c) = State ((Level c (openDoors4 (getColors (f c)) f)):ls) Play ds
                        | isExit (f c) = State (l:ls) Win []--Level (Coords 100 100) f
                        | otherwise = State ((Level c f):ls) Play ds 
                        where
                            isButton :: Tile -> Bool
                            isButton (Button _) = True
                            isButton _ = False
                            getColors :: Tile -> [DoorColor]
                            getColors (Button c') = [c']
                            getColors _ = []
                    newS = checkF (tryMove (stringToDir (unpack key)) l)
            handleWorld5 _ s = s
            drawWorld5 :: State -> Picture
            drawWorld5 (State _ Prepare _) = lettering "Loading... Press any btn to continue"
            drawWorld5 (State _ Win _) = lettering "Level completed! Press any btn to continue"
            drawWorld5 (State ((Level c f):_) _ _) = scaled (0.75) (0.75) (
                translated x y (drawPlayerAt c <> drawLevelMap f))
                where
                    sizeX = 21 -- later would be easier to change map size
                    sizeY = 21
                    x = - sizeX / 2
                    y = - sizeY / 2
            drawWorld5 _ = blank
    
    run :: IO ()
    run = solution5
    
    
