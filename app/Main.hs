{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List.Safe            ((!!))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Unique               as U
import           Prelude                   hiding ((!!))
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
  moveCursor (row + 1) cok

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
  , controller = Human handlePlayerInput
  , unique = u
               }


newNPC :: U.Unique -> Point -> Actor
newNPC u pos = Actor {
    point = pos
  , nextTurn = 2
  , speed = 3
  , controller = Basic idiotController
  , unique = u
               }

main :: IO ()
main = runCurses $ do
  setEcho False
  setCursorMode CursorInvisible
  scr <- newWindow 20 80 0 0
  logscr <- newWindow 10 80 19 0
  uniq <- liftIO U.newUnique
  uniq' <- liftIO U.newUnique
  levelText <- liftIO $ readFile "level.txt"
  let dungeon = Level.parseLevel $ T.pack levelText
  let p = newPlayer uniq
      npc = newNPC uniq' $ Point (3, 1)
  let gs = createGameState { level = dungeon }
      gs' = addActor npc $ addActor p gs
  run gs' (scr, logscr)


getNextTurn :: GameState -> Maybe (Actor, U.Unique, ActorQueue)
getNextTurn gs = do
  (u, aq) <- getNextActor gs
  a <- M.lookup u (actors gs)
  return (a, u, aq)



-- a more flexible way of doing this would be to use threads and MVars, I think.
-- ... maybe.
-- getAction :: GameState -> Window -> ActorController -> Curses (Maybe Action)
-- getAction gs scr ac = case ac of
--   Human hc -> do
--     ev <- getEvent scr Nothing
--     return $ fmap Input ev >>= hc
--   Basic bc -> return $ bc Trigger
--   AI aic ->



idiotController :: BasicController
idiotController _ = Just $ Say "I'm a dumbass NPC"


getAction :: GameState -> Window -> Actor -> Curses (Maybe Action)
getAction gs scr a = case controller a of
  Human hc -> do
    ev <- getEvent scr Nothing
    return $ fmap Input ev >>= hc
  Basic bc -> return $ bc Trigger
  -- AI aic -> do
  --   let (action, aic') = runTurn aic aic Trigger
  --   return action



-- this is a goddamned mess. Should refactor everything about this function
takeTurn :: GameState -> Window -> Curses GameState
takeTurn gs scr = do
  let next = getNextActor gs
  case next of
    Nothing -> return gs
    Just (u, aq) -> do
      let a = M.lookup u (actors gs)
      case a of
        Nothing -> return gs
        Just a' -> do
          action <- getAction gs scr (controller a')
          let eff = action >>= validateAction (level gs) a'
              gs' = case eff of Just e -> updateGameState e gs
                                Nothing -> gs
              a'' = M.lookup u (actors gs')
          case a'' of
            Nothing -> return gs
            Just a3  -> return $ updateActorTime a3 $ gs' { actorQueue = aq }


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



-- basically, the whole game flow/turn taking part needs to be rewritten,
-- with special thought to make human and AI players indistinguishable
-- to the game on this level.
run :: GameState -> (Window, Window) -> Curses ()
run gs (scr, logscr) = do
  let turn = isTurnNow gs

  gs' <- if turn then takeTurn gs scr else return gs { gameTime = gameTime gs + 1 }

  when turn $ drawGame scr gs'
  when turn $ drawLogWindow logscr gs'

  render

  run gs' (scr, logscr)
