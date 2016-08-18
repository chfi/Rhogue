{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GameState where
    -- ( GameState
    -- , Point
    -- ) where


import           Data.List.Safe ((!!))
import           Prelude        hiding ((!!))
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique


import Level
import Actor


-- would be nice to have the type of effect actually in the type...
data Effect =
    ActorEffect (Unique, (Actor -> Actor))
  -- | LogEffect (GameLog -> GameLog)
  | LogEffect Text
  | LevelEffect (Level -> Level)
  | MultiEffect [Effect]
  -- deriving (Eq)

instance Show Effect where
  show (ActorEffect _) = "ActorEffect"
  show (LogEffect _) = "LogEffect"
  show (LevelEffect _) = "LevelEffect"


-- moveActor :: Point -> Actor -> Actor
moveActor :: Unique -> Point -> Effect
moveActor u p = ActorEffect (u, \a -> a { point = addPoints (point a) p
                                        , nextTurn = speed a + nextTurn a })


addToLog :: Text -> Effect
addToLog t = LogEffect t


-- addLogPrefix :: Text -> Effect -> Effect
-- addLogPrefix p (LogEffect t) = LogEffect $ \t' -> p : t'
-- addLogPrefix _ _ = error "Attempt to add log prefix to other effect"



type GameLog = [Text]
type ActorQueue = MinPQueue Integer Unique

data GameState = GameState
  { actors :: Map Unique Actor
  , actorQueue :: MinPQueue Integer Unique
  , level :: Level
  , gameTime :: Integer
  , gameLog :: GameLog
  }



-- returns true if someone should take a turn
isTurnNow :: GameState -> Bool
isTurnNow GameState{..} = case next of Nothing -> False
                                       Just (nt,_) -> gameTime >= nt
  where next = PQ.getMin actorQueue


-- if it isn't anyone's turn, Nothing
-- else Just (next turn ID, actorQueue with first element removed)


-- this could be replaced by PQ.minView
getNextActor :: GameState -> Maybe (Unique, ActorQueue)
-- getNextActor GameState{..} = PQ.minView actorQueue
getNextActor GameState{..} = do
  (nt,u) <- PQ.getMin actorQueue
  uniq <- if nt >= gameTime then Just u else Nothing
  return (uniq, PQ.deleteMin actorQueue)


-- i haven't quite thought this through.
-- how should changing the actorQueue work as the game progresses?
-- if there is an actor that will take their move, we should remove
-- it from the queue, replacing it after the corresponding effects
-- have been performed.
-- well, that makes sense. we grab it, shrinking the queue, and then
-- we have a Unique. we do the things with that, including updating
-- the gamestate, then grab the new actor from `actors`, take the
-- new `gameTime`, and insert into the queue.


validateAction :: Level -> Actor -> Action -> Maybe Effect
validateAction l actor action = case action of
  Wait -> Nothing
  Move d -> if isMoveValid l (point actor) d then Just $ moveActor u d else Nothing
    where u = unique actor
  Say t -> Just $ MultiEffect [addToLog t, moveActor u $ Point (0,0)]
    where u = unique actor



-- LogEffect :: GameLog -> GameLog

updateGameState :: Effect -> GameState -> GameState
updateGameState eff gs = case eff of
  ActorEffect (u,f) -> gs { actors = actors' }
    where actors' = M.adjust f u (actors gs)
  (LogEffect f) -> gs { gameLog = prefixed : (gameLog gs) }
    where prefixed = (T.pack $ "Turn " ++ (show (gameTime gs)) ++ ": ") `T.append` f
  LevelEffect f -> gs { level = f (level gs) }
  MultiEffect fs -> foldr updateGameState gs fs


createGameState :: GameState
createGameState = GameState
  { actors = M.empty
  , actorQueue = PQ.empty
  , level = emptyLevel
  , gameTime = 0
  , gameLog = []
  }


addActor :: Actor -> GameState -> GameState
addActor a gs = gs { actors = M.insert (unique a) a (actors gs)
                   , actorQueue = PQ.insert (nextTurn a) (unique a) (actorQueue gs)}

-- if the actor already has a turn, nothing happens.
updateActorTime :: Actor -> GameState -> GameState
updateActorTime a gs = gs { actorQueue = newQueue }
  where newQueue = PQ.insert (nextTurn a) (unique a) (actorQueue gs)
