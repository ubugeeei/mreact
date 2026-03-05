{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | The core @Hooks@ indexed free monad.
--
-- @Hooks i j a@ represents an effectful computation that:
--
--   * starts in hook-state @i@ (type-level list of 'Slot's)
--   * ends in hook-state @j@
--   * produces a value of type @a@
--
-- Each hook primitive prepends a slot to @j@, making conditional usage
-- a type error (both branches of an @if@ must produce the same @j@).
--
-- The sole exception is @use@ ('HUse'), which keeps @i ~ j@ — the identity
-- operation on the graded monoid. This matches React 19's semantics where
-- @use@ may appear inside conditionals and loops.
module MReact.Hooks
  ( -- * The Hooks GADT
    Hooks(..)
    -- * Re-exports of all hook constructors
  , module MReact.Hooks.State
  , module MReact.Hooks.Effect
  , module MReact.Hooks.Ref
  , module MReact.Hooks.Memo
  , module MReact.Hooks.Context
  , module MReact.Hooks.Async
  , module MReact.Hooks.Transition
  , module MReact.Hooks.Id
  ) where

import MReact.Indexed
import MReact.Types
import MReact.Fiber (Fiber)

import MReact.Hooks.State
import MReact.Hooks.Effect
import MReact.Hooks.Ref
import MReact.Hooks.Memo
import MReact.Hooks.Context
import MReact.Hooks.Async
import MReact.Hooks.Transition
import MReact.Hooks.Id

-- | The indexed free monad for React Hooks.
--
-- This is the "effectful computation" from the blog post:
--   Component : Props -> R(Fiber)
--
-- The GADT constructors correspond to algebraic effect operations,
-- and the runtime interpreter ('MReact.Runtime.Scheduler') is
-- the effect handler (React Fiber).
type Hooks :: [Slot] -> [Slot] -> * -> *
data Hooks i j a where
  -- | Pure value — identity on the state index.
  HPure   :: a -> Hooks i i a

  -- | Bind — chains state transitions: @i -> j -> k@ becomes @i -> k@.
  HBind   :: Hooks i j a -> (a -> Hooks j k b) -> Hooks i k b

  -- | Map — preserves state indices.
  HMap    :: (a -> b) -> Hooks i j a -> Hooks i j b

  ---------------------------------------------------------------------------
  -- Stateful hooks (non-identity index — Rules of Hooks enforced)
  ---------------------------------------------------------------------------

  -- | @useState@: allocate a state slot.
  HState  :: s -> Hooks i (SState s ': i) (s, SetState s)

  -- | @useReducer@: allocate a reducer slot.
  HReducer :: (s -> action -> s) -> s -> Hooks i (SReducer s ': i) (s, Dispatch action)

  -- | @useRef@: allocate a mutable ref slot.
  HRef    :: a -> Hooks i (SRef a ': i) (Ref a)

  -- | @useEffect@: schedule a side effect (runs after commit).
  HEffect :: Deps -> IO (IO ()) -> Hooks i (SEffect ': i) ()

  -- | @useLayoutEffect@: schedule a synchronous effect (runs before paint).
  HLayoutEffect :: Deps -> IO (IO ()) -> Hooks i (SLayoutEffect ': i) ()

  -- | @useMemo@: cache a computed value.
  HMemo   :: Deps -> (() -> a) -> Hooks i (SMemo a ': i) a

  -- | @useCallback@: cache a function (specialization of useMemo).
  HCallback :: Deps -> f -> Hooks i (SCallback f ': i) f

  -- | @useContext@: subscribe to a context provider.
  HContext :: Context a -> Hooks i (SContext a ': i) a

  -- | @useId@: generate a stable unique identifier.
  HId     :: Hooks i (SId ': i) String

  -- | @useTransition@: mark a state update as non-urgent.
  HTransition :: Hooks i (STransition ': i) TransitionHandle

  -- | @useDeferredValue@: defer a value update to a lower priority.
  HDeferred :: a -> Hooks i (SDeferredValue a ': i) a

  ---------------------------------------------------------------------------
  -- Identity-index operations (safe inside control flow)
  ---------------------------------------------------------------------------

  -- | @use@: unwrap an 'Async' (Promise).
  --
  -- __Index is identity__ (@i -> i@), so this may appear inside
  -- @if@\/@case@\/@guard@ without causing a type error.
  -- This is the key insight from the indexed monad discussion:
  -- @use : Async a -> Hooks 1 a@ where @1@ is the monoid unit.
  --
  -- When the 'Async' is 'Pending', the interpreter throws a
  -- 'SuspendException'. The nearest 'HSuspense' boundary catches
  -- it and renders the fallback instead.
  HUse    :: Async a -> Hooks i i a

  -- | @suspense@: catch suspension from child @use@ calls.
  --
  -- __Index is identity__ — Suspense boundaries do not allocate
  -- hook slots, so they may appear inside control flow.
  --
  -- When the child computation throws 'SuspendException' (because
  -- a @use@ encountered a 'Pending' async), the fallback 'Fiber'
  -- is returned instead. Once the async resolves, a re-render is
  -- scheduled and the child renders normally.
  HSuspense :: Fiber -> Hooks i i Fiber -> Hooks i i Fiber

instance IxFunctor Hooks where
  imap = HMap

instance IxApplicative Hooks where
  ipure = HPure
  iap mf ma = HBind mf (\f -> HMap f ma)

instance IxMonad Hooks where
  ibind = HBind
