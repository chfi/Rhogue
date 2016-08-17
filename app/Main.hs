{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where


import Control.Monad
import           Control.Monad.IO.Class
import           Data.List.Safe         ((!!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Unique            as U
import           Prelude                hiding ((!!))
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
  , nextTurn = 5
  , speed = 1
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
  let p = newPlayer uniq
  let gs = createGameState { level = dungeon }
      gs' = addActor p gs
  run gs' (scr, logscr)
  -- return ()
  -- run (GameState (newPlayer uniq) dungeon 0 [T.pack "hello world"]) (scr, logscr)
  -- run (GameState (newPlayer uniq) emptyLevel 0 [T.pack "hello world"]) (scr, logscr)




takeTurn :: GameState -> Window -> Curses GameState
takeTurn gs scr = do
{--
rough pseudocode:
if there is a nextActor (we _know_ there is at this point...)
get that actor's action via their actorcontroller.
if the action is nothing, fail out; return the original gamestate.
if there is an action, perform it on the gamestate.
  also update the actorqueue

so, if there _is_ a next actor, then the game should "pause".
otherwise, it should _always_ step forward.
--}

  let next = getNextActor gs
  case next of
    Nothing -> return gs
    Just (u, aq) -> do
      let a = M.lookup u (actors gs)
      case a of
        Nothing -> return gs
        Just a' -> do
          ev <- getEvent scr Nothing
          let eff = (fmap Input ev >>= (controller a')) >>= validateAction (level gs) a'
              gs' = case eff of Just e -> updateGameState e gs
                                Nothing -> gs
              a'' = M.lookup u (actors gs')
          case a'' of
            Nothing -> return gs
            Just a3  -> return $ updateActorTime a3 $ gs' { gameLog = T.pack ("Turn: actor " ++
                                                                       (show $ U.hashUnique (unique a3))) : gameLog gs'
                                                          , actorQueue = aq}
          -- case ev of
          --   Nothing -> return gs
          --   Just ev ->



drawGame :: Window -> GameState -> Curses ()
drawGame scr gs' =
  updateWindow scr $ do
    clear
    drawLevel (level gs')
    mapM_ drawPlayer (actors gs')


drawLogWindow :: Window -> GameState -> Curses ()
drawLogWindow logscr gs' =
  updateWindow logscr $ do
    clear
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 1 1
    drawLog (gameLog gs')


run :: GameState -> (Window, Window) -> Curses ()
run gs (scr, logscr) = do
  let turn = isTurnNow gs

  gs' <- if turn then takeTurn gs scr else return gs { gameLog = (T.pack $ show $ gameTime gs) : gameLog gs
                                                     , gameTime = gameTime gs + 1}

  when turn $ drawGame scr gs'
  when turn $ drawLogWindow logscr gs'

  render

  run gs' (scr, logscr)
