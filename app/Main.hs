module Main where


import           Data.List.Safe ((!!))
import           Prelude        hiding ((!!))
import           UI.NCurses


import           Lib


type Point = (Integer, Integer)


data Tile =
    Floor
  | Wall
  | Player
  deriving (Eq)

type World = [[Tile]]


data PlayerAction =
    Move Point
  | Wait
  deriving (Eq, Show)


data GameState = GameState
  { playerPoint :: Point
  , world       :: World
  , turnNumber  :: Integer
  , history     :: [String]
  } deriving (Eq, Show)


-- type History = [String]
-- type TurnNumber = Integer


instance Show Tile where
  show Floor = "."
  show Wall = "#"
  show Player = "@"

makeFloor = repeat Floor
makeWall = repeat Wall

-- ugliest map in the history of video games??
dungeon :: World
dungeon = [take 80 makeFloor
          ,take 5 makeWall ++ take 75 makeFloor ++ take 70 makeFloor
          ,take 80 makeFloor
          ,take 80 makeFloor
          ,take 80 makeFloor
          ,take 5 makeFloor ++ take 5 makeWall ++ take 70 makeFloor
          ,take 5 makeFloor ++ [Wall] ++ take 3 makeFloor ++ [Wall] ++ take 70 makeFloor
          ,take 9 makeFloor ++ [Wall] ++ take 70 makeFloor
          ,take 5 makeFloor ++ [Wall] ++ take 3 makeFloor ++ [Wall] ++ take 70 makeFloor
          ,take 5 makeFloor ++ [Wall] ++ take 3 makeFloor ++ [Wall] ++ take 70 makeFloor
          ,take 5 makeFloor ++ [Wall] ++ take 3 makeFloor ++ [Wall] ++ take 70 makeFloor
          ,take 5 makeFloor ++ [Wall] ++ take 3 makeFloor ++ [Wall] ++ take 70 makeFloor
          ,take 5 makeFloor ++ take 5 makeWall ++ take 70 makeFloor
          ,take 80 makeFloor
          ,take 80 makeFloor
          -- ,take 80 makeFloor
          ,take 30 makeFloor ++ take 20 makeWall ++ take 30 makeFloor
          ,take 80 makeFloor
          ,take 80 makeFloor
          ,take 80 makeFloor
          ]

getTile :: World -> Point -> Maybe Tile
getTile w (x,y) = do
  row <- w !! fromInteger x
  row !! fromInteger y

isPassable :: World -> Point -> Bool
isPassable w p = case getTile w p of
  Just Floor -> True
  Nothing -> True
  _ -> False


-- having "addPoints p m" at two points like this smells bad...
-- there's probably a nicer way of solving this
validateAction :: World -> Point -> PlayerAction -> Maybe PlayerAction
validateAction w p pa = case pa of
  Wait -> Just pa
  Move m -> if isPassable w p' then Just pa else Nothing
    where p' = addPoints p m

updatePlayer :: Point -> PlayerAction -> Point
updatePlayer p pa = case pa of
  Move m -> addPoints p m
  Wait -> p


addPoints :: Point -> Point -> Point
addPoints (x1,y1) (x2,y2) = (x1+x2, y1+y2)


-- look into the ViewPatterns GHC extension for these things
playerMove :: Event -> Maybe PlayerAction
playerMove ev = case ev of
  EventSpecialKey KeyUpArrow    -> Just $ Move (-1, 0)
  EventSpecialKey KeyDownArrow  -> Just $ Move (1,   0)
  EventSpecialKey KeyLeftArrow  -> Just $ Move (0,  -1)
  EventSpecialKey KeyRightArrow -> Just $ Move (0,    1)
  _ -> Nothing

drawTile :: Tile -> Update ()
drawTile i = drawString $ show i

drawRow :: [Tile] -> Update ()
drawRow r = do
  (row,col) <- cursorPosition
  mapM_ drawTile r
  moveCursor (row + 1) col

drawWorld :: World -> Update ()
drawWorld = mapM_ drawRow

drawPlayer :: Point -> Update ()
drawPlayer p = do
  uncurry moveCursor p
  drawTile Player


drawStringLine :: String -> Update ()
drawStringLine s = do
  (row,col) <- cursorPosition
  drawString s
  moveCursor (row + 1) col

drawLog :: [String] -> Update ()
drawLog history = do
  (rows,cols) <- windowSize
  let logmax = fromInteger (rows - 2)
  mapM_ drawStringLine $ take logmax history


pos :: Point
pos = (8,3)

main :: IO ()
main = runCurses $ do
  setEcho False
  setCursorMode CursorInvisible
  scr <- newWindow 20 80 0 0
  logscr <- newWindow 10 80 19 0
  run (GameState pos dungeon 0 ["hello world"]) (scr, logscr)


-- GameState should use the state monad - clean this up a bit
-- at the very least use more record syntax
-- use RecordWildcards
updateGameState :: Maybe PlayerAction -> GameState -> GameState
updateGameState pa gs = GameState p' w' (t+1) (show pa : history gs)
  where p = playerPoint gs
        w' = world gs
        t = turnNumber gs
        p' = case pa of Nothing -> p
                        Just m -> updatePlayer p m


run :: GameState -> (Window, Window) -> Curses ()
run gs (scr, logscr) = do
  ev <- getEvent scr Nothing
  let pa = (ev >>= playerMove) >>= validateAction (world gs) (playerPoint gs)
      gs' = updateGameState pa gs
  updateWindow scr $ do
    clear
    drawWorld (world gs')
    drawPlayer (playerPoint gs')

  updateWindow logscr $ do
    clear
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 1 1
    drawLog (history gs')
  render
  run gs' (scr, logscr)
