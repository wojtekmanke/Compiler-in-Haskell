-- | Liveness analysis.
--
-- A variable is /live/ at a point of the program if its current value may
-- still be read later. The analysis is the classic backward data-flow
-- problem, solved over the control-flow graph of a whole function:
--
-- > liveOut(B) = union of liveIn(S) over the successors S of B
-- > liveIn(B)  = (liveOut(B) - defined(B)) + usedBeforeDefined(B)
module Latte.Quadruples.Liveness
  ( Live (..)
  , liveness
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Quadruples

-- | What is live in one basic block.
data Live = Live
  { liveIn    :: Set.Set Var    -- ^ On entry to the block.
  , liveAfter :: [Set.Set Var]  -- ^ After each quad of the block, in order.
  , liveOut   :: Set.Set Var    -- ^ On exit from the block.
  }

-- | Computes liveness for each block of a function.
liveness :: [BasicBlock] -> [Live]
liveness blocks = map describe blocks
  where
    entries = liveOnEntry blocks
    describe block =
      let out = liveOnExit entries block
          first :| rest = liveWithin block out
      in Live { liveIn = first, liveAfter = rest, liveOut = out }

-- | The live variables at the start of every block: the least fixed point of
-- the data-flow equations, reached by iterating from empty sets.
liveOnEntry :: [BasicBlock] -> Map.Map Label (Set.Set Var)
liveOnEntry blocks = go (Map.fromList [(blockLabel block, Set.empty) | block <- blocks])
  where
    go entries
      | updated == entries = entries
      | otherwise          = go updated
      where
        updated = foldr update entries blocks
    -- Liveness flows backwards, so visiting blocks last to first converges fastest.
    update block entries =
      let entry = NonEmpty.head (liveWithin block (liveOnExit entries block))
      in Map.insert (blockLabel block) entry entries

liveOnExit :: Map.Map Label (Set.Set Var) -> BasicBlock -> Set.Set Var
liveOnExit entries block =
  Set.unions [Map.findWithDefault Set.empty target entries | target <- successors (blockEnd block)]

-- | The live variables before each quad of a block and, as the last element,
-- before its terminator, given what is live on exit.
liveWithin :: BasicBlock -> Set.Set Var -> NonEmpty (Set.Set Var)
liveWithin (BasicBlock _ quads end) out = NonEmpty.scanr before beforeEnd quads
  where
    beforeEnd = out `Set.union` Set.fromList (terminatorUses end)
    before quad after =
      maybe id Set.delete (definition quad) after `Set.union` Set.fromList (uses quad)
