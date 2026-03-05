{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Core types used throughout MReact.
module MReact.Types
  ( -- * Hook slot kinds
    Slot(..)
    -- * Dependency tracking
  , Deps(..)
  , noDeps
  , emptyDeps
  , deps
    -- * Ref
  , Ref(..)
  , newRef
  , readRef
  , writeRef
  , modifyRef
    -- * Async (Promise)
  , Async(..)
  , async
  , resolve
    -- * Context
  , Context(..)
  , createContext
    -- * State setter
  , SetState
  , Dispatch
    -- * Transition
  , TransitionHandle(..)
  ) where

import Data.IORef
import Data.Kind (Type)

--------------------------------------------------------------------------------
-- Hook slot kinds — phantom types that annotate the indexed monad
--------------------------------------------------------------------------------

-- | Each constructor represents a distinct hook slot type.
-- When a hook is called, it prepends a @Slot@ to the type-level state list.
-- @use@ is the only API that does NOT add a slot — it has identity index.
data Slot
  = SState Type         -- ^ @useState@: mutable state of type @a@
  | SReducer Type       -- ^ @useReducer@: reducer state of type @a@
  | SRef Type           -- ^ @useRef@: mutable ref of type @a@
  | SMemo Type          -- ^ @useMemo@: cached value of type @a@
  | SCallback Type      -- ^ @useCallback@: cached function of type @a@
  | SEffect             -- ^ @useEffect@: scheduled side effect
  | SLayoutEffect       -- ^ @useLayoutEffect@: synchronous DOM effect
  | SContext Type       -- ^ @useContext@: context subscription
  | SId                 -- ^ @useId@: stable unique identifier
  | STransition         -- ^ @useTransition@: concurrent transition
  | SDeferredValue Type -- ^ @useDeferredValue@: deferred value

--------------------------------------------------------------------------------
-- Dependency tracking
--------------------------------------------------------------------------------

-- | Dependencies for effects and memoization, mirroring React's deps array.
data Deps
  = NoDeps                       -- ^ No dependency array — run every render
  | EmptyDeps                    -- ^ Empty array @[]@ — run only on mount
  | forall d. Eq d => Deps d     -- ^ Dependency value — run when value changes

noDeps :: Deps
noDeps = NoDeps

emptyDeps :: Deps
emptyDeps = EmptyDeps

deps :: Eq d => d -> Deps
deps = Deps

--------------------------------------------------------------------------------
-- Ref (mutable reference, like React's useRef)
--------------------------------------------------------------------------------

-- | A mutable reference, equivalent to React's @{ current: T }@.
newtype Ref a = MkRef { unRef :: IORef a }

newRef :: a -> IO (Ref a)
newRef = fmap MkRef . newIORef

readRef :: Ref a -> IO a
readRef = readIORef . unRef

writeRef :: Ref a -> a -> IO ()
writeRef ref = writeIORef (unRef ref)

modifyRef :: Ref a -> (a -> a) -> IO ()
modifyRef ref = modifyIORef' (unRef ref)

--------------------------------------------------------------------------------
-- Async (Promise)
--------------------------------------------------------------------------------

-- | An asynchronous computation, analogous to JavaScript's @Promise@.
-- The @use@ API unwraps this with identity index, so it may appear
-- inside control flow without violating Rules of Hooks.
newtype Async a = Async { runAsync :: IO a }

-- | Create an Async from an IO action.
async :: IO a -> Async a
async = Async

-- | Create an already-resolved Async (like @Promise.resolve@).
resolve :: a -> Async a
resolve = Async . pure

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

-- | A context value with a default, analogous to React's @createContext@.
data Context a = Context
  { contextDefault :: a
  , contextId      :: !Int
  }

-- | Create a new context with a default value.
-- In a real implementation, @contextId@ would be globally unique.
createContext :: a -> Context a
createContext a = Context a 0

--------------------------------------------------------------------------------
-- State setter / Dispatch
--------------------------------------------------------------------------------

-- | State update function for @useState@.
-- Takes the next state value directly (State as a Snapshot).
type SetState s = s -> IO ()

-- | Action dispatch function for @useReducer@.
type Dispatch action = action -> IO ()

--------------------------------------------------------------------------------
-- Transition
--------------------------------------------------------------------------------

-- | Handle returned by @useTransition@.
data TransitionHandle = TransitionHandle
  { isPending      :: !Bool
  , startTransition :: IO () -> IO ()
  }
