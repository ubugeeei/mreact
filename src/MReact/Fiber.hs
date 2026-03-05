{-# LANGUAGE OverloadedStrings #-}

-- | Fiber node types.
--
-- With @OverloadedStrings@, string literals can be used directly as 'Fiber':
--
-- @
-- h1 [] ["Hello, world!"]
-- @
--
-- This is the target of component rendering: @Component : Props -> R(Fiber)@.
-- The fiber tree is then diffed against the previous tree, producing patches
-- that are applied to the real DOM.
module MReact.Fiber
  ( -- * Fiber node
    Fiber(..)
  , Key
    -- * Attributes and events
  , Attrs
  , EventName
  , EventHandler
  , DOMEvent(..)
    -- * Constructors
  , ftext
  , felement
  , fkeyed
  , ffragment
  , fnull
    -- * Queries
  , isNull
  , nodeTag
  , nodeChildren
  , nodeAttrs
    -- * Equality (for idempotency)
  , fiberEq
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Key for reconciliation (like React's @key@ prop).
type Key = String

-- | HTML attribute name -> value.
type Attrs = Map String String

-- | Event name (e.g., "click", "input", "submit").
type EventName = String

-- | Opaque DOM event.
data DOMEvent = DOMEvent
  { eventType   :: String
  , eventTarget :: String   -- ^ Serialized target info
  } deriving (Show, Eq)

-- | Event handler callback.
type EventHandler = DOMEvent -> IO ()

-- | Fiber node — the unit of the UI tree.
data Fiber
  = FText !String
    -- ^ Text node
  | FElement !String !Attrs !(Map EventName EventHandler) ![Fiber]
    -- ^ Element with tag, attributes, event handlers, and children
  | FKeyed !String !Attrs !(Map EventName EventHandler) ![(Key, Fiber)]
    -- ^ Keyed element for stable list reconciliation
  | FFragment ![Fiber]
    -- ^ Fragment (multiple root nodes, like React's @<>...</>@)
  | FNull
    -- ^ Null element (renders nothing, like @null@ in JSX)

-- | Show instance ignores event handlers (which are functions).
instance Show Fiber where
  show (FText s)              = "FText " ++ show s
  show (FElement t a _ cs)    = "FElement " ++ show t ++ " " ++ show a ++ " " ++ show cs
  show (FKeyed t a _ cs)      = "FKeyed " ++ show t ++ " " ++ show a ++ " " ++ show (map (\(k,v) -> (k, v)) cs)
  show (FFragment cs)         = "FFragment " ++ show cs
  show FNull                  = "FNull"

-- | Eq instance uses structural equality (ignores event handlers).
instance Eq Fiber where
  (==) = fiberEq

-- | String literals become 'FText' nodes with @OverloadedStrings@.
instance IsString Fiber where
  fromString = FText

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

ftext :: String -> Fiber
ftext = FText

felement :: String -> Attrs -> Map EventName EventHandler -> [Fiber] -> Fiber
felement = FElement

fkeyed :: String -> Attrs -> Map EventName EventHandler -> [(Key, Fiber)] -> Fiber
fkeyed = FKeyed

ffragment :: [Fiber] -> Fiber
ffragment = FFragment

fnull :: Fiber
fnull = FNull

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

isNull :: Fiber -> Bool
isNull FNull = True
isNull _     = False

nodeTag :: Fiber -> Maybe String
nodeTag (FElement t _ _ _) = Just t
nodeTag (FKeyed t _ _ _)   = Just t
nodeTag _                  = Nothing

nodeChildren :: Fiber -> [Fiber]
nodeChildren (FElement _ _ _ cs) = cs
nodeChildren (FFragment cs)      = cs
nodeChildren _                   = []

nodeAttrs :: Fiber -> Attrs
nodeAttrs (FElement _ a _ _) = a
nodeAttrs (FKeyed _ a _ _)   = a
nodeAttrs _                  = Map.empty

--------------------------------------------------------------------------------
-- Structural equality (ignoring event handlers, which are functions)
--
-- This is used to verify the idempotency property:
--   u_s . u_s = u_s
-- If render(s) produces structurally equal fiber trees, the reconciler
-- produces no patches, making the operation idempotent.
--------------------------------------------------------------------------------

-- | Structural equality of fiber trees, ignoring event handlers.
fiberEq :: Fiber -> Fiber -> Bool
fiberEq (FText a)              (FText b)              = a == b
fiberEq (FElement t1 a1 _ c1)  (FElement t2 a2 _ c2)  = t1 == t2 && a1 == a2 && listEq c1 c2
fiberEq (FKeyed t1 a1 _ c1)    (FKeyed t2 a2 _ c2)    = t1 == t2 && a1 == a2 && keyedEq c1 c2
fiberEq (FFragment c1)         (FFragment c2)          = listEq c1 c2
fiberEq FNull                  FNull                   = True
fiberEq _                      _                       = False

listEq :: [Fiber] -> [Fiber] -> Bool
listEq []     []     = True
listEq (x:xs) (y:ys) = fiberEq x y && listEq xs ys
listEq _      _      = False

keyedEq :: [(Key, Fiber)] -> [(Key, Fiber)] -> Bool
keyedEq []          []          = True
keyedEq ((k1,v1):xs) ((k2,v2):ys) = k1 == k2 && fiberEq v1 v2 && keyedEq xs ys
keyedEq _           _           = False
