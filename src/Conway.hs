{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Conway where

import Data.Functor.Compose (Compose (..))
import qualified Data.Vector as V
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Arrow ((***))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))
import Control.Concurrent

----------------------------------------------------------------------------------------

-- | Game loop

tickTime :: Int
tickTime = 200000

run :: IO()

run = loop (step basicRule) start

loop :: (Grid Bool -> Grid Bool) -> Grid Bool -> IO ()
loop stepper g = do
    putStr "\ESC[2J"
    putStrLn (render g)
    threadDelay tickTime
    loop stepper (stepper g)

----------------------------------------------------------------------------------------

-- | Implementation of the fixed-sized 20 * 20 board and instance of bounded vector

newtype VBounded a = VBounded (V.Vector a)
    deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
    distribute = distributeRep

gridSize :: Int
gridSize = 20

instance Representable VBounded where
    type Rep VBounded = Int
    index (VBounded v) i = v V.! (i `mod` gridSize)
    tabulate desc = VBounded $ V.generate gridSize desc

----------------------------------------------------------------------------------------

-- | Let's wrap it up in a store

type Grid a = Store (Compose VBounded VBounded) a
type Coord = (Int, Int)

mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup (0, 0)
    where
        lookup crd = crd `elem` xs

----------------------------------------------------------------------------------------

-- | The rule of our game of life

type Rule = Grid Bool -> Bool

neighbourCoords :: [(Int, Int)]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

basicRule :: Rule
basicRule g = 
    (alive && numNeighboursAlive `elem` [2, 3]) || (not alive && numNeighboursAlive == 3)
    where
        alive = extract g
        addCoords (x, y) (x', y') = (x + x', y + y')
        neighbours = experiment (\s -> addCoords s <$> neighbourCoords) g
        numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid Bool -> Grid Bool
step = extend

----------------------------------------------------------------------------------------

-- | Render our board to text

render :: Grid Bool -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#"))g

at :: [Coord] -> Coord -> [Coord]
at xs (x, y) = fmap ((+x) *** (+y)) xs

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

start :: Grid Bool
start = mkGrid $
    glider `at` (0, 0)
 ++ beacon `at` (15, 5)
 ++ blinker `at` (16, 4)

