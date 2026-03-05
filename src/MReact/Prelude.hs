{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | User-facing prelude for writing MReact components.
--
-- Import this module with @RebindableSyntax@ to get indexed do-notation
-- that enforces Rules of Hooks at the type level:
--
-- @
-- {-\# LANGUAGE RebindableSyntax \#-}
-- {-\# LANGUAGE DataKinds \#-}
-- module MyComponent where
--
-- import MReact.Prelude
--
-- counter :: FC '[] '[ 'SState Int] ()
-- counter () = do
--   (count, setCount) <- useState 0
--   return $ div []
--     [ text ("Count: " ++ show count)
--     , button [onClick (\\_ -> setCount (+ 1))] ["+"]
--     ]
-- @
--
-- The @do@ notation is rebound to the indexed monad's @ibind@,
-- so each @<-@ is a Kleisli bind that tracks state transitions.
-- Using @useState@ inside an @if@ is a __type error__:
--
-- @
-- -- Type error: 'SState Int ': i /= i
-- bad () = do
--   if condition
--     then do { (n, _) <- useState 0; ... }
--     else return nullElem
-- @
module MReact.Prelude
  ( -- * Rebindable syntax support
    -- $rebindable
    return
  , (>>=)
  , (>>)
  , ifThenElse
  , fail

    -- * Re-exports: Types
  , module MReact.Types

    -- * Re-exports: Hooks GADT & smart constructors
  , Hooks(..)
  , useState
  , useReducer
  , useRef
  , useEffect
  , useEffectWithCleanup
  , useLayoutEffect
  , useLayoutEffectWithCleanup
  , useMemo
  , useCallback
  , useContext
  , useId
  , useTransition
  , useDeferredValue
  , use

    -- * Re-exports: Component types (includes suspense)
  , module MReact.Component

    -- * Re-exports: VDOM
  , VNode(..)
  , vdomEq

    -- * Re-exports: OverloadedStrings support
  , IsString(..)

    -- * Re-exports: DOM DSL (JSX equivalent)
  , module MReact.DOM

    -- * Standard Prelude (without conflicting names)
  , module Prelude
  ) where

-- We hide Prelude's monad operations so RebindableSyntax picks up ours
import Prelude hiding (return, (>>=), (>>), fail, div, span, id, head, map, pred)
import qualified Prelude

import MReact.Types
import MReact.Hooks (Hooks(..))
import MReact.Component
import MReact.VDOM (VNode(..), vdomEq)
import Data.String (IsString(..))
import MReact.DOM

-- $rebindable
-- These functions replace the standard Prelude versions when
-- @{-\# LANGUAGE RebindableSyntax \#-}@ is enabled. They
-- operate on the indexed 'Hooks' monad instead of regular monads.

-- | Identity bind: @return a@ has index @i -> i@ (no state change).
-- This is the @pure@ / @eta@ of the indexed monad.
return :: a -> Hooks i i a
return = HPure

-- | Indexed bind: chains @Hooks i j a@ and @(a -> Hooks j k b)@ into @Hooks i k b@.
-- This is the Kleisli composition that accumulates hook slots in the type.
(>>=) :: Hooks i j a -> (a -> Hooks j k b) -> Hooks i k b
(>>=) = HBind

-- | Indexed sequence: like @(>>=)@ but discards the first result.
(>>) :: Hooks i j a -> Hooks j k b -> Hooks i k b
m >> k = HBind m (\_ -> k)

-- | Required by RebindableSyntax for pattern match failures in do-notation.
fail :: String -> Hooks i j a
fail = error

-- | Required by RebindableSyntax for @if-then-else@ in do-blocks.
-- This is the regular boolean conditional — both branches must have
-- the __same type__, which enforces that they use the same hooks.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

--------------------------------------------------------------------------------
-- Hook smart constructors
--
-- These are the actual user-facing API. They construct the GADT values
-- that the runtime interpreter will handle.
--------------------------------------------------------------------------------

-- | Allocate a mutable state slot. Index: @i -> 'SState s ': i@.
--
-- The returned setter takes the next state value directly (State as a Snapshot):
-- @setCount (count + 1)@, not @setCount (+ 1)@.
--
-- __Cannot be used inside control flow__ — doing so is a type error.
useState :: s -> Hooks i (SState s ': i) (s, SetState s)
useState = HState

-- | Allocate a reducer state slot. Index: @i -> 'SReducer s ': i@.
useReducer :: (s -> action -> s) -> s -> Hooks i (SReducer s ': i) (s, Dispatch action)
useReducer = HReducer

-- | Allocate a mutable ref slot. Index: @i -> 'SRef a ': i@.
useRef :: a -> Hooks i (SRef a ': i) (Ref a)
useRef = HRef

-- | Schedule a side effect (runs after commit). Index: @i -> 'SEffect ': i@.
useEffect :: Deps -> IO () -> Hooks i (SEffect ': i) ()
useEffect d eff = HEffect d (eff Prelude.>> Prelude.pure (Prelude.pure ()))

-- | Like 'useEffect' but with a cleanup function.
--
-- The @IO (IO ())@ is: effect body returns cleanup action.
useEffectWithCleanup :: Deps -> IO (IO ()) -> Hooks i (SEffect ': i) ()
useEffectWithCleanup = HEffect

-- | Schedule a synchronous effect (runs before paint). Index: @i -> 'SLayoutEffect ': i@.
useLayoutEffect :: Deps -> IO () -> Hooks i (SLayoutEffect ': i) ()
useLayoutEffect d eff = HLayoutEffect d (eff Prelude.>> Prelude.pure (Prelude.pure ()))

-- | Like 'useLayoutEffect' but with a cleanup function.
useLayoutEffectWithCleanup :: Deps -> IO (IO ()) -> Hooks i (SLayoutEffect ': i) ()
useLayoutEffectWithCleanup = HLayoutEffect

-- | Cache a computed value. Index: @i -> 'SMemo a ': i@.
useMemo :: Deps -> (() -> a) -> Hooks i (SMemo a ': i) a
useMemo = HMemo

-- | Cache a function. Index: @i -> 'SCallback f ': i@.
useCallback :: Deps -> f -> Hooks i (SCallback f ': i) f
useCallback = HCallback

-- | Subscribe to a context. Index: @i -> 'SContext a ': i@.
useContext :: Context a -> Hooks i (SContext a ': i) a
useContext = HContext

-- | Generate a stable unique ID. Index: @i -> 'SId ': i@.
useId :: Hooks i (SId ': i) String
useId = HId

-- | Start a non-urgent transition. Index: @i -> 'STransition ': i@.
useTransition :: Hooks i (STransition ': i) TransitionHandle
useTransition = HTransition

-- | Defer a value. Index: @i -> 'SDeferredValue a ': i@.
useDeferredValue :: a -> Hooks i (SDeferredValue a ': i) a
useDeferredValue = HDeferred

-- | Unwrap an 'Async' (Promise). __Index: i -> i__ (identity!).
--
-- This is the only hook-like API that may appear inside @if@, @case@,
-- and loops. Both branches of a conditional have matching indices
-- because @use@ does not allocate a hook slot.
--
-- When the 'Async' is 'Pending', the interpreter throws a
-- 'SuspendException'. Wrap the call site in 'suspense' to catch
-- suspensions and display a fallback.
use :: Async a -> Hooks i i a
use = HUse

-- suspense is re-exported from MReact.Component
