{-# LANGUAGE OverloadedStrings #-}

module Actor where


import           Data.Text  (Text)
import qualified Data.Text  as T
import Data.Unique (Unique)
import qualified Data.Unique as U
import           UI.NCurses


-- import Effect
import Level
-- import GameState (Point)

data Action =
    Move Point
  | Wait
  | Say Text
  deriving (Eq, Show)

-- An actor is anything that can take turns on its own,
-- including the player.


-- a player character has an event,
-- but an NPC only needs to know that it's their turn.
-- alternatively, we could have that contain the "memory" of the AI...
--   overkill for now. work on what matters.
data ControlInput = Input Event | Trigger



type Memory = Integer


-- this doesn't make sense. An actor should be able to have
-- either of these types of controllers - we need a sum type
-- containing them
type HumanController = ControlInput -> Maybe Action
type BasicController = ControlInput -> Maybe Action
type IOController = IO (Maybe Action)
-- type AIController a = a -> ControlInput -> (Maybe Action, a)


data AIController a = AIController { helper :: a -> ControlInput -> (Maybe Action, a)
                                   , runner :: ControlInput -> (Maybe Action, a)}
-- type AIController = a -> (ControlInput -> Maybe Action, a)
-- type AIController = ControlInput -> (s -> (Maybe Action, s))
-- type AIController = State Memory ActorController

-- an AI should really have some greater knowledge of the game,
-- possibly specific to only that actor...
-- I suppose that technically could be done by MVars etc.

-- type AIController = a -> ControlInput -> (Maybe Action, a -> ControlInput (Maybe Action, b))
-- data AIController s =
--   AIController { memory :: s
--                , runTurn :: ControlInput -> (s -> (Maybe Action, s))
--                }



data ActorController =
    Human HumanController
  | Basic BasicController
  | AI AIController

-- look into the ViewPatterns GHC extension for these things
handlePlayerInput :: HumanController
handlePlayerInput Trigger = Nothing
handlePlayerInput (Input ev) = case ev of
  EventSpecialKey KeyUpArrow    -> Just $ Move $ Point (0, -1)
  EventSpecialKey KeyDownArrow  -> Just $ Move $ Point (0,  1)
  EventSpecialKey KeyLeftArrow  -> Just $ Move $ Point (-1, 0)
  EventSpecialKey KeyRightArrow -> Just $ Move $ Point (1,  0)
  EventCharacter ' ' -> Just $ Say "hello"
  _ -> Nothing


-- handleAITurn :: AIController
-- handleAITurn t
--   | t `mod` 2 == 0 = (\_ -> Just $ Move $ Point (-1, 0), t + 1)
--   | t `mod` 3 == 0 = (\_ -> Just $ Move $ Point (1, 0), t + 1)
--   | otherwise = (const Nothing, t + 1)


data Actor = Actor
  { point      :: Point
  , nextTurn   :: Integer
  , speed      :: Integer
  , controller :: ActorController
  , unique     :: Unique
  }
