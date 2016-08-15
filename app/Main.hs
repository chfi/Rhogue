{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Data.List.Safe ((!!))
import           Prelude        hiding ((!!))
import           UI.NCurses
import Data.Text (Text)
import qualified Data.Text as T


import           GameState




makeFloor = repeat Floor
makeWall = repeat Wall

-- ugliest map in the history of video games??
dungeon :: Level
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



-- look into the ViewPatterns GHC extension for these things
handlePlayerInput :: Event -> Maybe Action
handlePlayerInput ev = case ev of
  EventSpecialKey KeyUpArrow    -> Just $ Move (-1, 0)
  EventSpecialKey KeyDownArrow  -> Just $ Move (1,   0)
  EventSpecialKey KeyLeftArrow  -> Just $ Move (0,  -1)
  EventSpecialKey KeyRightArrow -> Just $ Move (0,    1)
  EventCharacter ' ' -> Just $ Say "hello"
  _ -> Nothing


drawTile :: Tile -> Update ()
drawTile i = drawString $ show i

drawRow :: [Tile] -> Update ()
drawRow r = do
  (row,col) <- cursorPosition
  mapM_ drawTile r
  moveCursor (row + 1) col

drawLevel :: Level -> Update ()
drawLevel = mapM_ drawRow

drawPlayer :: Actor -> Update ()
drawPlayer (Actor p) = do
  uncurry moveCursor p
  drawTile Player


drawStringLine :: Text -> Update ()
drawStringLine s = do
  (row,col) <- cursorPosition
  drawString (T.unpack s)
  moveCursor (row + 1) col

drawLog :: [Text] -> Update ()
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
  run (GameState (Actor pos) dungeon 0 [T.pack "hello world"]) (scr, logscr)


run :: GameState -> (Window, Window) -> Curses ()
run gs (scr, logscr) = do
  -- the earliest of beginnings of a time system

  ev <- if turnNumber gs `mod` 2 == 0 then getEvent scr Nothing
                                      else return Nothing

  let eff = (ev >>= handlePlayerInput) >>= validateAction (level gs) (player gs)
      gs' = case eff of Just e -> updateGameState e gs
                        Nothing -> gs
  updateWindow scr $ do
    clear
    drawLevel (level gs')
    drawPlayer (player gs')

  updateWindow logscr $ do
    clear
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 1 1
    drawLog (gameLog gs')
  render
  run (gs' {turnNumber = turnNumber gs' + 1}) (scr, logscr)
