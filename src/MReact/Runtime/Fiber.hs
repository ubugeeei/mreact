{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Fiber data structure — the runtime state for a mounted component.
--
-- Each fiber holds:
--   * A hook store (the memoized state from each hook call)
--   * A cursor tracking the current hook position during rendering
--   * The previously rendered VDOM (for diffing)
--   * Effect queues (for commit phase)
--
-- This corresponds to React Fiber in the algebraic effects model:
-- the fiber IS the effect handler's internal state.
module MReact.Runtime.Fiber
  ( -- * Fiber
    Fiber(..)
  , newFiber
  , resetCursor
    -- * Hook store
  , HookStore
  , SomeHookValue(..)
  , readHook
  , writeHook
  , appendHook
    -- * Effect entry
  , EffectEntry(..)
  , EffectPhase(..)
    -- * Render context
  , RenderCtx(..)
  , newRenderCtx
  ) where

import Data.IORef
import MReact.VDOM (VNode(..))

--------------------------------------------------------------------------------
-- Existential wrapper for hook values
--------------------------------------------------------------------------------

-- | Type-erased hook value. The indexed monad guarantees that the nth hook
-- always has the same type across renders, so casting is safe.
data SomeHookValue = forall a. SomeHookValue a

--------------------------------------------------------------------------------
-- Hook store
--------------------------------------------------------------------------------

-- | Mutable array of hook values, indexed by position.
type HookStore = IORef [SomeHookValue]

readHook :: HookStore -> Int -> IO SomeHookValue
readHook store i = do
  hooks <- readIORef store
  if i < length hooks
    then pure (hooks !! i)
    else error $ "MReact.Runtime.Fiber: hook index out of bounds: " ++ show i

writeHook :: HookStore -> Int -> SomeHookValue -> IO ()
writeHook store i val = modifyIORef' store $ \hooks ->
  let (before, _:after) = splitAt i hooks
  in before ++ [val] ++ after

appendHook :: HookStore -> SomeHookValue -> IO Int
appendHook store val = do
  hooks <- readIORef store
  let idx = length hooks
  writeIORef store (hooks ++ [val])
  pure idx

--------------------------------------------------------------------------------
-- Effect entry
--------------------------------------------------------------------------------

-- | Which phase to run the effect in.
data EffectPhase = PassiveEffect | LayoutEffect
  deriving (Show, Eq)

-- | A scheduled effect with its cleanup.
data EffectEntry = EffectEntry
  { effectPhase   :: !EffectPhase
  , effectAction  :: IO (IO ())     -- ^ Effect body (returns cleanup)
  , effectCleanup :: IORef (IO ())  -- ^ Stored cleanup from last run
  , effectDeps    :: IORef (Maybe [SomeHookValue])
    -- ^ Previous deps (Nothing = first run)
  }

--------------------------------------------------------------------------------
-- Fiber
--------------------------------------------------------------------------------

-- | A Fiber represents a mounted component instance.
data Fiber = Fiber
  { fiberHookStore    :: !HookStore
    -- ^ All hook values, indexed by call order
  , fiberCursor       :: !(IORef Int)
    -- ^ Current hook index (reset to 0 before each render)
  , fiberIsFirstRender :: !(IORef Bool)
    -- ^ True during the initial render
  , fiberPrevVDOM     :: !(IORef VNode)
    -- ^ VDOM from the previous render (for diffing)
  , fiberEffects      :: !(IORef [EffectEntry])
    -- ^ Pending effects (passive and layout)
  , fiberIdCounter    :: !(IORef Int)
    -- ^ Counter for useId
  , fiberScheduleUpdate :: IO ()
    -- ^ Callback to schedule a re-render of this fiber
  }

-- | Create a new fiber for a component.
newFiber :: IO () -> IO Fiber
newFiber scheduleUpdate = do
  store   <- newIORef []
  cursor  <- newIORef 0
  first   <- newIORef True
  prev    <- newIORef VNull
  effects <- newIORef []
  idCtr   <- newIORef 0
  pure Fiber
    { fiberHookStore     = store
    , fiberCursor        = cursor
    , fiberIsFirstRender = first
    , fiberPrevVDOM      = prev
    , fiberEffects       = effects
    , fiberIdCounter     = idCtr
    , fiberScheduleUpdate = scheduleUpdate
    }

-- | Reset the hook cursor to 0 (called before each render pass).
resetCursor :: Fiber -> IO ()
resetCursor fiber = writeIORef (fiberCursor fiber) 0

--------------------------------------------------------------------------------
-- Render context (threaded through the interpreter)
--------------------------------------------------------------------------------

-- | Context values available during rendering.
data RenderCtx = RenderCtx
  { ctxValues :: [(Int, SomeHookValue)]
    -- ^ Context id -> value mapping (from Provider ancestors)
  }

newRenderCtx :: RenderCtx
newRenderCtx = RenderCtx []
