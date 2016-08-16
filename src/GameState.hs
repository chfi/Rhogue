{-# LANGUAGE OverloadedStrings #-}

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
    ActorEffect (Actor -> Actor)
  | LogEffect (GameLog -> GameLog)
  | LevelEffect (Level -> Level)
  | MultiEffect [Effect]
  -- deriving (Eq)

instance Show Effect where
  show (ActorEffect _) = "ActorEffect"
  show (LogEffect _) = "LogEffect"
  show (LevelEffect _) = "LevelEffect"


-- moveActor :: Point -> Actor -> Actor
moveActor :: Point -> Effect
moveActor p = ActorEffect $ \a -> a { point = addPoints (point a) p
                                    , nextTurn = 3 + nextTurn a }


addToLog :: Text -> Effect
addToLog t = LogEffect (t :)


addLogPrefix :: Text -> Effect -> Effect
addLogPrefix p (LogEffect t) = LogEffect $ \t' -> p : t'
addLogPrefix _ _ = error "Attempt to add log prefix to other effect"



type GameLog = [Text]

data GameState = GameState
  { --actors :: Map Unique Actor
  player :: Actor
  --, actorQueue :: MinPQueue Integer Unique
  , level :: Level
  , gameTime :: Integer
  , gameLog :: GameLog
  }




validateAction :: Level -> Actor -> Action -> Maybe Effect
validateAction l actor action = case action of
  Wait -> Nothing
  Move d -> if isMoveValid l (point actor) d then Just $ moveActor d else Nothing
  Say t -> Just $ addToLog t


updateGameState :: Effect -> GameState -> GameState
updateGameState eff gs = case eff of
  ActorEffect f -> gs { player = f (player gs) }
  f@(LogEffect _) -> gs { gameLog = prefixed (gameLog gs) }
    where (LogEffect prefixed) = addLogPrefix (T.pack (show (gameTime gs))) f
  LevelEffect f -> gs { level = f (level gs) }
  MultiEffect fs -> foldr updateGameState gs fs
