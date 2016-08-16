{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where


import Control.Monad.IO.Class
import           Data.List.Safe ((!!))
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Unique as U
import           Prelude        hiding ((!!))
import           UI.NCurses


import           Actor
import           GameState
import           Level


drawTile :: Tile -> Update ()
drawTile i = drawString $ show i

drawRow :: [Tile] -> Update ()
drawRow r = do
  (row,col) <- cursorPosition
  mapM_ drawTile r
  moveCursor (row + 1) col

drawLevel :: Level -> Update ()
drawLevel l = mapM_ drawRow $ Level.levelToList l

drawPlayer :: Actor -> Update ()
drawPlayer Actor { point = Point (x,y) } = do
  uncurry moveCursor (y,x)
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
pos = Point (1,1)


newPlayer :: U.Unique -> Actor
newPlayer u = Actor {
    point = pos
  , nextTurn = 0
  , controller = handlePlayerInput
  , unique = u
               }


main :: IO ()
main = runCurses $ do
  setEcho False
  setCursorMode CursorInvisible
  scr <- newWindow 20 80 0 0
  logscr <- newWindow 10 80 19 0
  uniq <- liftIO U.newUnique
  levelText <- liftIO $ readFile "level.txt"
  let dungeon = Level.parseLevel $ T.pack levelText
  run (GameState (newPlayer uniq) dungeon 0 [T.pack "hello world"]) (scr, logscr)
  -- run (GameState (newPlayer uniq) emptyLevel 0 [T.pack "hello world"]) (scr, logscr)


run :: GameState -> (Window, Window) -> Curses ()
run gs (scr, logscr) = do
  -- the earliest of beginnings of a time system

  ev <- if gameTime gs `mod` 2 == 0 then getEvent scr Nothing
                                      else return Nothing

  let eff = ((fmap Input ev) >>= handlePlayerInput) >>= validateAction (level gs) (player gs)
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
  run (gs' {gameTime = gameTime gs' + 1}) (scr, logscr)
