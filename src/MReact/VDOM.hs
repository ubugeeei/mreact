{-# LANGUAGE OverloadedStrings #-}

-- | Virtual DOM types.
--
-- With @OverloadedStrings@, string literals can be used directly as 'VNode':
--
-- @
-- h1 [] ["Hello, world!"]
-- @
--
-- This is the target of component rendering: @Component : Props -> R(VDOM)@.
-- The VDOM tree is then diffed against the previous tree, producing patches
-- that are applied to the real DOM.
module MReact.VDOM
  ( -- * VDOM node
    VNode(..)
  , Key
    -- * Attributes and events
  , Attrs
  , EventName
  , EventHandler
  , DOMEvent(..)
    -- * Constructors
  , vtext
  , velement
  , vkeyed
  , vfragment
  , vnull
    -- * Queries
  , isNull
  , nodeTag
  , nodeChildren
  , nodeAttrs
    -- * Equality (for idempotency)
  , vdomEq
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

-- | Virtual DOM node.
data VNode
  = VText !String
    -- ^ Text node
  | VElement !String !Attrs !(Map EventName EventHandler) ![VNode]
    -- ^ Element with tag, attributes, event handlers, and children
  | VKeyed !String !Attrs !(Map EventName EventHandler) ![(Key, VNode)]
    -- ^ Keyed element for stable list reconciliation
  | VFragment ![VNode]
    -- ^ Fragment (multiple root nodes, like React's @<>...</>@)
  | VNull
    -- ^ Null element (renders nothing, like @null@ in JSX)

-- | Show instance ignores event handlers (which are functions).
instance Show VNode where
  show (VText s)              = "VText " ++ show s
  show (VElement t a _ cs)    = "VElement " ++ show t ++ " " ++ show a ++ " " ++ show cs
  show (VKeyed t a _ cs)      = "VKeyed " ++ show t ++ " " ++ show a ++ " " ++ show (map (\(k,v) -> (k, v)) cs)
  show (VFragment cs)         = "VFragment " ++ show cs
  show VNull                  = "VNull"

-- | Eq instance uses structural equality (ignores event handlers).
instance Eq VNode where
  (==) = vdomEq

-- | String literals become 'VText' nodes with @OverloadedStrings@.
instance IsString VNode where
  fromString = VText

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

vtext :: String -> VNode
vtext = VText

velement :: String -> Attrs -> Map EventName EventHandler -> [VNode] -> VNode
velement = VElement

vkeyed :: String -> Attrs -> Map EventName EventHandler -> [(Key, VNode)] -> VNode
vkeyed = VKeyed

vfragment :: [VNode] -> VNode
vfragment = VFragment

vnull :: VNode
vnull = VNull

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

isNull :: VNode -> Bool
isNull VNull = True
isNull _     = False

nodeTag :: VNode -> Maybe String
nodeTag (VElement t _ _ _) = Just t
nodeTag (VKeyed t _ _ _)   = Just t
nodeTag _                  = Nothing

nodeChildren :: VNode -> [VNode]
nodeChildren (VElement _ _ _ cs) = cs
nodeChildren (VFragment cs)      = cs
nodeChildren _                   = []

nodeAttrs :: VNode -> Attrs
nodeAttrs (VElement _ a _ _) = a
nodeAttrs (VKeyed _ a _ _)   = a
nodeAttrs _                  = Map.empty

--------------------------------------------------------------------------------
-- Structural equality (ignoring event handlers, which are functions)
--
-- This is used to verify the idempotency property:
--   u_s . u_s = u_s
-- If render(s) produces structurally equal VDOMs, the reconciler
-- produces no patches, making the operation idempotent.
--------------------------------------------------------------------------------

-- | Structural equality of VDOM trees, ignoring event handlers.
vdomEq :: VNode -> VNode -> Bool
vdomEq (VText a)              (VText b)              = a == b
vdomEq (VElement t1 a1 _ c1)  (VElement t2 a2 _ c2)  = t1 == t2 && a1 == a2 && listEq c1 c2
vdomEq (VKeyed t1 a1 _ c1)    (VKeyed t2 a2 _ c2)    = t1 == t2 && a1 == a2 && keyedEq c1 c2
vdomEq (VFragment c1)         (VFragment c2)          = listEq c1 c2
vdomEq VNull                  VNull                   = True
vdomEq _                      _                       = False

listEq :: [VNode] -> [VNode] -> Bool
listEq []     []     = True
listEq (x:xs) (y:ys) = vdomEq x y && listEq xs ys
listEq _      _      = False

keyedEq :: [(Key, VNode)] -> [(Key, VNode)] -> Bool
keyedEq []          []          = True
keyedEq ((k1,v1):xs) ((k2,v2):ys) = k1 == k2 && vdomEq v1 v2 && keyedEq xs ys
keyedEq _           _           = False
