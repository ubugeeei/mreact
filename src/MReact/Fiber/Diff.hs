-- | Fiber diff algorithm.
--
-- Computes a minimal set of 'Patch'es to transform one fiber tree into another.
-- This is the "reconcile" step in the pipeline:
--
-- @
-- u_s = commit . reconcile . render(s)
-- @
--
-- The key property is that when @render(s)@ produces structurally equal
-- fiber trees (which it does for the same state), @reconcile@ produces
-- an empty patch list, making the overall operation idempotent:
--
-- @
-- u_s . u_s = u_s
-- @
module MReact.Fiber.Diff
  ( -- * Patch types
    Patch(..)
  , AttrPatch(..)
  , Path
    -- * Diff algorithm
  , diff
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MReact.Fiber

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Path into the DOM tree (list of child indices from root).
type Path = [Int]

-- | An attribute-level patch.
data AttrPatch
  = SetAttr  !String !String    -- ^ Set attribute to value
  | RemoveAttr !String          -- ^ Remove attribute
  deriving (Show, Eq)

-- | A patch operation to apply to the real DOM.
data Patch
  = PReplace    !Path !Fiber
    -- ^ Replace the entire node at this path
  | PRemove     !Path
    -- ^ Remove the node at this path
  | PInsert     !Path !Int !Fiber
    -- ^ Insert a new child at the given index
  | PText       !Path !String
    -- ^ Update text content
  | PAttrs      !Path ![AttrPatch]
    -- ^ Update attributes
  | PReorder    !Path ![Int]
    -- ^ Reorder children (keyed reconciliation)
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Diff algorithm
--------------------------------------------------------------------------------

-- | Compute patches to transform @old@ fiber tree into @new@ fiber tree.
-- An empty result means the trees are identical — no DOM mutation needed.
diff :: Fiber -> Fiber -> [Patch]
diff old new = diffAt [] old new

diffAt :: Path -> Fiber -> Fiber -> [Patch]

-- Same text
diffAt _    (FText a)    (FText b)    | a == b = []
diffAt path (FText _)    (FText b)    = [PText path b]

-- Same element tag — diff attributes and children
diffAt path (FElement t1 a1 _ c1) (FElement t2 a2 _ c2)
  | t1 == t2  = diffAttrs path a1 a2 ++ diffChildren path c1 c2
  | otherwise = [PReplace path (FElement t2 a2 mempty c2)]

-- Keyed elements
diffAt path (FKeyed t1 a1 _ c1) (FKeyed t2 a2 _ c2)
  | t1 == t2  = diffAttrs path a1 a2 ++ diffKeyed path c1 c2
  | otherwise = [PReplace path (FKeyed t2 a2 mempty c2)]

-- Fragments
diffAt path (FFragment c1) (FFragment c2) = diffChildren path c1 c2

-- Null
diffAt _    FNull FNull = []

-- Type mismatch — full replace
diffAt path _ new = [PReplace path new]

--------------------------------------------------------------------------------
-- Attribute diff
--------------------------------------------------------------------------------

diffAttrs :: Path -> Attrs -> Attrs -> [Patch]
diffAttrs path old new
  | old == new = []
  | otherwise  = [PAttrs path patches]
  where
    patches = removed ++ changed
    removed = [ RemoveAttr k | k <- Map.keys old, not (Map.member k new) ]
    changed = [ SetAttr k v
              | (k, v) <- Map.toList new
              , Map.lookup k old /= Just v
              ]

--------------------------------------------------------------------------------
-- Children diff (positional)
--------------------------------------------------------------------------------

diffChildren :: Path -> [Fiber] -> [Fiber] -> [Patch]
diffChildren path old new = updates ++ removals ++ insertions
  where
    common = zip3 [0..] old new
    updates = concatMap (\(i, o, n) -> diffAt (path ++ [i]) o n) common

    oldLen = length old
    newLen = length new

    removals
      | oldLen > newLen = [ PRemove (path ++ [i]) | i <- reverse [newLen .. oldLen - 1] ]
      | otherwise       = []

    insertions
      | newLen > oldLen = [ PInsert path i n | (i, n) <- zip [oldLen..] (drop oldLen new) ]
      | otherwise       = []

--------------------------------------------------------------------------------
-- Keyed diff (stable identity)
--------------------------------------------------------------------------------

diffKeyed :: Path -> [(Key, Fiber)] -> [(Key, Fiber)] -> [Patch]
diffKeyed path old new
  | map fst old == map fst new =
      -- Same key order: just diff each pair
      concatMap (\(i, (_, o), (_, n)) -> diffAt (path ++ [i]) o n)
               (zip3 [0..] old new)
  | otherwise =
      -- Key order changed: compute reorder + per-element diffs
      let oldMap = Map.fromList old
          newMap = Map.fromList new
          -- Diff elements that exist in both
          shared = [ diffAt (path ++ [i]) (oldMap Map.! k) n
                   | (i, (k, n)) <- zip [0..] new
                   , Map.member k oldMap
                   ]
          -- Removed keys
          removedKeys = [ k | (k, _) <- old, not (Map.member k newMap) ]
          removePatches = [ PRemove (path ++ [i])
                          | (i, (k, _)) <- zip [0..] old
                          , k `elem` removedKeys
                          ]
          -- New key order (for reorder)
          newOrder = [ maybe (-1) id (lookup k indexed)
                     | (k, _) <- new
                     ]
            where indexed = zip (map fst old) [0..]
      in concat shared ++ removePatches ++ [PReorder path newOrder]
