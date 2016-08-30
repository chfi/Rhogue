-- simple AI that moves an actor back and forth between two adjacent tiles

module AI.Bot.BackAndForth where


import Debug.Trace
import Data.IORef
-- import Actor
-- import Effect (Action)
-- import Level (Point)


data Direction = North | East | South | West deriving (Show)
data Step = Back | Forth deriving (Eq, Ord)
data Memory = Memory Direction Step


revDir :: Direction -> Direction
revDir North = South
revDir South = North
revDir West = East
revDir East = West


-- wait shit this doesn't work either.
-- it'll mutate the AI state even if the action turns out to be invalid.
-- goddamn it.

-- I should just have the getAction function in Main modify the GameState,
-- and return Just (Maybe Action, GameState)...
-- Or, return Just (Maybe Action, Actor), and update based on the actor.
-- that'd be cleaner, at least.
backAndForth' :: IORef -> ControlInput -> IO (Maybe Action)
backAndForth' ior _ = do
  d <- readIORef ior
  writeIORef ior (revDir d)
  return $ Just $ Move $ dirToPoint d

backAndForth dir = do
  ior <- newIORef dir
  return $ backAndForth' ior
