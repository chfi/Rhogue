module Effect where


import Data.Text (Text)
import Data.Unique (Unique)
-- import Actor (Actor)
import Actor
-- import GameState
import Level (Level
             ,Point)




data Effect =
    ActorEffect (Unique, Actor -> Actor)
  -- | LogEffect (GameLog -> GameLog)
  | LogEffect Text
  | LevelEffect (Level -> Level)
  | MultiEffect [Effect]
  -- deriving (Eq)

data ActionError =
    MovementBlocked
  | IllegalAction -- this is just a placeholder
  deriving (Eq, Show)


effectOfAction :: GameState -> Actor -> Action -> Either ActionError Effect
effectOfAction gs actor action = case action of
  Wait -> Right $ actorWait actor
  Move d -> if isMoveValid (level gs) (point actor) d then Right $ moveActor actor d
                                                      else Left MovementBlocked
    where u = unique actor
  Say t -> Right $ MultiEffect [addToLog t, moveActor actor $ Point (0,0)]
    where u = unique actor



-- Specific Effect functions
moveActor :: Actor -> Point -> Effect
moveActor a p = ActorEffect (unique a, \a -> a { point = addPoints (point a) p
                                               , nextTurn = speed a + nextTurn a })

actorWait :: Actor -> Effect
actorWait a = ActorEffect (unique a, \a -> a { nextTurn = 1 + nextTurn a })

addToLog :: Text -> Effect
addToLog = LogEffect
