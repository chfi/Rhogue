module Level where


import Prelude hiding ((!!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.Safe ((!!))
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

newtype Point = Point (Integer, Integer) deriving (Eq, Show)


-- reverse lexographic ordering so we order by rows first, not columns
instance Ord Point where
  Point (a,b) `compare` Point (c,d) =
    if b /= d then b `compare` d else a `compare` c
  -- Point (a,b) `compare` Point (c,d) =
  --   if a /= c then a `compare` c else b `compare` d

addPoints :: Point -> Point -> Point
addPoints (Point (x1,y1)) (Point (x2,y2)) = Point (x1+x2, y1+y2)

type Level = Map Point Tile


emptyLevel :: Level
emptyLevel = M.empty



data Tile =
    Floor
  | Wall
  | Player
  deriving (Eq)


instance Show Tile where
  show Floor = "."
  show Wall = "#"
  show Player = "@"


levelToList :: Level -> [[Tile]]
levelToList l = (fmap . fmap) snd rows
  where al = M.toAscList l
        rows = L.groupBy (\(Point (_, y1), _)
                           (Point (_, y2), _)
                                         -> y1 == y2) al


getTile :: Level -> Point -> Maybe Tile
-- getTile l (Point (x,y)) = M.lookup (Point (y,x)) l
getTile l (Point (x,y)) = M.lookup (Point (x,y)) l


isPointPassable :: Level -> Point -> Bool
isPointPassable l p = case getTile l p of
  Just Floor -> True
  Nothing -> True
  _ -> False


isMoveValid :: Level -> Point -> Point -> Bool
isMoveValid l a p = isPointPassable l $ addPoints a p

parseTile :: Char -> Tile
parseTile '#' = Wall
parseTile  _  = Floor

parseLine :: Text -> [(Integer, Tile)]
parseLine = zip [0..] . foldr (\v l -> parseTile v : l) [] . T.unpack

parseLines :: [Text] -> [(Integer, [(Integer, Tile)])]
parseLines = zip [0..] . map parseLine

parseToTiles :: [Text] -> [(Point, Tile)]
parseToTiles ts = do
  (y, row) <- parseLines ts
  (x, t) <- row
  return (Point (x, y), t)

parseLevel :: Text -> Level
parseLevel = foldr (\(p,t) l -> M.insert p t l) M.empty . parseToTiles . T.lines
