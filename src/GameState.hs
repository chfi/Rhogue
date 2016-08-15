{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GameState where
    -- ( GameState
    -- , Point
    -- ) where


import           Data.List.Safe ((!!))
import           Prelude        hiding ((!!))
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import qualified Data.Text as T


import Actor
-- import Level

-- type Point = (Integer, Integer)

type Level = [[Tile]]

addPoints :: Point -> Point -> Point
addPoints (x1,y1) (x2,y2) = (x1+x2, y1+y2)


data Tile =
    Floor
  | Wall
  | Player
  deriving (Eq)


instance Show Tile where
  show Floor = "."
  show Wall = "#"
  show Player = "@"


-- should Move be relative or absolute?
-- data Action =
--     Move Point
--   | Wait
--   | Say Text
--   deriving (Eq, Show)


-- newtype ActorEffect = ActorEffect (Actor -> Actor)
-- newtype LogEffect = LogEffect (Log -> Log)


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
moveActor p = ActorEffect $ \a -> a { point = addPoints (point a) p }


-- addToLog :: Text -> GameLog -> GameLog
addToLog :: Text -> Effect
addToLog t = LogEffect (t :)


addLogPrefix :: Text -> Effect -> Effect
addLogPrefix p (LogEffect t) = LogEffect $ \t' -> p : t'
addLogPrefix _ _ = error "Attempt to add log prefix to other effect"


-- setTile :: Point -> Tile -> Effect
-- setTile p l =


newtype ValidAction = Valid (Actor, Action) deriving (Eq, Show)

data Actor = Actor
  { point :: Point }
  deriving (Eq, Show)


type GameLog = [Text]

data GameState = GameState
  { player :: Actor
  , level :: Level
  , turnNumber :: Integer
  , gameLog :: GameLog
  }
  deriving (Eq, Show)



getTile :: Level -> Point -> Maybe Tile
getTile l (x,y) = do
  row <- l !! fromInteger x
  row !! fromInteger y

isPointPassable :: Level -> Point -> Bool
isPointPassable l p = case getTile l p of
  Just Floor -> True
  Nothing -> True
  _ -> False


isMoveValid :: Level -> Actor -> Point -> Bool
isMoveValid l a p = isPointPassable l $ addPoints (point a) p


validateAction :: Level -> Actor -> Action -> Maybe Effect
validateAction l actor action = case action of
  Wait -> Nothing
  Move d -> if isMoveValid l actor d then Just $ moveActor d else Nothing
  Say t -> Just $ addToLog t


updateGameState :: Effect -> GameState -> GameState
updateGameState eff gs = case eff of
  ActorEffect f -> gs { player = f (player gs) }
  f@(LogEffect _) -> gs { gameLog = prefixed (gameLog gs) }
    where (LogEffect prefixed) = addLogPrefix (T.pack (show (turnNumber gs))) f
  LevelEffect f -> gs { level = f (level gs) }
  MultiEffect fs -> foldr updateGameState gs fs
