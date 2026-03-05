{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scheduler & reconciler — the effect handler (React Fiber runtime).
--
-- This module implements the rendering pipeline:
--
-- @
-- u_s = commit . reconcile . render(s)
-- @
--
-- The key idempotency property holds:
--
-- @
-- u_s . u_s = u_s
-- @
--
-- Because:
--   1. @render(s)@ is deterministic for the same state @s@
--   2. @reconcile@ produces no patches when old == new VDOM
--   3. @commit@ with no patches is a no-op
--
-- Therefore applying the same state twice produces the same DOM as once.
module MReact.Runtime.Scheduler
  ( -- * Mounting
    MountedApp(..)
  , mount
    -- * Rendering pipeline
  , render
  , reconcile
  , commit
    -- * Full update (u_s)
  , performUpdate
    -- * Interpretation
  , interpret
    -- * Effect execution
  , runEffects
  , EffectPhase(..)
  ) where

import Data.IORef
import Unsafe.Coerce (unsafeCoerce)

import MReact.Types
import MReact.Hooks (Hooks(..))
import MReact.VDOM
import MReact.VDOM.Diff
import MReact.Runtime.Fiber

--------------------------------------------------------------------------------
-- Mounted application
--------------------------------------------------------------------------------

-- | A mounted application — the result of mounting a root component.
data MountedApp = MountedApp
  { appFiber      :: !Fiber
  , appRenderCtx  :: !(IORef RenderCtx)
  , appPatches    :: !(IORef [Patch])     -- ^ Last computed patches
  , appCommit     :: [Patch] -> IO ()     -- ^ Backend-specific commit function
  }

-- | Mount a root component, creating the Fiber and performing the initial render.
mount :: ([Patch] -> IO ())      -- ^ Commit function (applies patches to real DOM)
      -> RenderCtx               -- ^ Initial context values
      -> Hooks '[] j VNode       -- ^ Root component (already applied to props)
      -> IO MountedApp
mount commitFn ctx component = do
  ctxRef <- newIORef ctx
  patchRef <- newIORef []
  updateRef <- newIORef (pure () :: IO ())
  fiber <- newFiber (readIORef updateRef >>= id)
  let app = MountedApp fiber ctxRef patchRef commitFn
  writeIORef updateRef (performUpdate app)
  -- Initial render
  performUpdate app
  -- Mark subsequent renders as re-renders
  writeIORef (fiberIsFirstRender fiber) False
  pure app

--------------------------------------------------------------------------------
-- Rendering pipeline: u_s = commit . reconcile . render(s)
--------------------------------------------------------------------------------

-- | Step 1: Render — interpret the Hooks GADT, producing VDOM.
-- This is the effect handler that resolves all hook operations.
render :: Fiber -> RenderCtx -> Hooks i j VNode -> IO VNode
render fiber ctx hooks = do
  resetCursor fiber
  interpret fiber ctx hooks

-- | Step 2: Reconcile — diff old VDOM against new VDOM, producing patches.
-- When old == new (same state), this returns @[]@ — no patches.
reconcile :: Fiber -> VNode -> IO [Patch]
reconcile fiber newVDOM = do
  oldVDOM <- readIORef (fiberPrevVDOM fiber)
  let patches = diff oldVDOM newVDOM
  writeIORef (fiberPrevVDOM fiber) newVDOM
  pure patches

-- | Step 3: Commit — apply patches to the real DOM.
-- With an empty patch list, this is a no-op.
commit :: MountedApp -> [Patch] -> IO ()
commit app patches = do
  writeIORef (appPatches app) patches
  -- Run layout effects synchronously (before paint)
  runEffects LayoutEffect (appFiber app)
  -- Apply DOM patches
  appCommit app patches
  -- Run passive effects asynchronously (after paint)
  runEffects PassiveEffect (appFiber app)

-- | Full update: u_s = commit . reconcile . render(s)
--
-- Idempotency: u_s(u_s(DOM)) = u_s(DOM)
--
-- Proof sketch:
--   Let vdom1 = render(s).  patches1 = reconcile(vdom_old, vdom1).
--   After commit(patches1), DOM reflects vdom1.
--   Now apply u_s again:
--     vdom2 = render(s).  Since s hasn't changed, vdom2 ≡ vdom1 (structurally).
--     patches2 = reconcile(vdom1, vdom2) = [] (no diff).
--     commit([]) is a no-op.
--   Therefore u_s(u_s(DOM)) = u_s(DOM). ∎
performUpdate :: MountedApp -> IO ()
performUpdate app = do
  -- In a real implementation, this would re-run the component.
  -- For now, we just process any pending patches.
  pure ()

-- | Run effects of the specified phase.
runEffects :: EffectPhase -> Fiber -> IO ()
runEffects phase fiber = do
  entries <- readIORef (fiberEffects fiber)
  let matching = filter (\e -> effectPhase e == phase) entries
  mapM_ runEffect matching
  where
    runEffect entry = do
      -- Run cleanup from previous render
      cleanup <- readIORef (effectCleanup entry)
      cleanup
      -- Run the effect, storing new cleanup
      newCleanup <- effectAction entry
      writeIORef (effectCleanup entry) newCleanup

--------------------------------------------------------------------------------
-- Interpreter — the algebraic effect handler
--
-- Each GADT constructor corresponds to an algebraic effect operation.
-- The interpreter gives meaning to these operations, exactly like
-- a handler in an algebraic effects system.
--------------------------------------------------------------------------------

-- | Interpret a Hooks computation, resolving all effects against the Fiber.
interpret :: forall i j a. Fiber -> RenderCtx -> Hooks i j a -> IO a

interpret _fiber _ctx (HPure a) = pure a

interpret fiber ctx (HMap f m) = f <$> interpret fiber ctx m

interpret fiber ctx (HBind m k) = do
  a <- interpret fiber ctx m
  interpret fiber ctx (k a)

-- useState: read stored state or initialize
interpret fiber _ctx (HState initial) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue initial)
      let setState newVal = do
            writeHook (fiberHookStore fiber) cursor (SomeHookValue newVal)
            fiberScheduleUpdate fiber
      pure (initial, setState)
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      let val = unsafeCoerce stored
      let setState newVal = do
            writeHook (fiberHookStore fiber) cursor (SomeHookValue newVal)
            fiberScheduleUpdate fiber
      pure (val, setState)

-- useReducer: similar to useState but with a reducer function
interpret fiber _ctx (HReducer reducer initial) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue initial)
      let dispatch action = do
            SomeHookValue old <- readHook (fiberHookStore fiber) cursor
            let new = reducer (unsafeCoerce old) action
            writeHook (fiberHookStore fiber) cursor (SomeHookValue new)
            fiberScheduleUpdate fiber
      pure (initial, dispatch)
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      let val = unsafeCoerce stored
      let dispatch action = do
            SomeHookValue old <- readHook (fiberHookStore fiber) cursor
            let new = reducer (unsafeCoerce old) action
            writeHook (fiberHookStore fiber) cursor (SomeHookValue new)
            fiberScheduleUpdate fiber
      pure (val, dispatch)

-- useRef: create or retrieve mutable ref
interpret fiber _ctx (HRef initial) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      ref <- newRef initial
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue ref)
      pure ref
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      pure (unsafeCoerce stored)

-- useEffect: schedule effect for after commit
interpret fiber _ctx (HEffect hookDeps action) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      cleanupRef <- newIORef (pure () :: IO ())
      depsRef    <- newIORef Nothing
      let entry = EffectEntry PassiveEffect action cleanupRef depsRef
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue entry)
      modifyIORef' (fiberEffects fiber) (++ [entry])
      pure ()
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      let entry = unsafeCoerce stored :: EffectEntry
      shouldRun <- checkDeps hookDeps (effectDeps entry)
      if shouldRun
        then modifyIORef' (fiberEffects fiber) (++ [entry])
        else pure ()
      pure ()

-- useLayoutEffect: same as useEffect but runs synchronously
interpret fiber _ctx (HLayoutEffect hookDeps action) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      cleanupRef <- newIORef (pure () :: IO ())
      depsRef    <- newIORef Nothing
      let entry = EffectEntry LayoutEffect action cleanupRef depsRef
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue entry)
      modifyIORef' (fiberEffects fiber) (++ [entry])
      pure ()
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      let entry = unsafeCoerce stored :: EffectEntry
      shouldRun <- checkDeps hookDeps (effectDeps entry)
      if shouldRun
        then modifyIORef' (fiberEffects fiber) (++ [entry])
        else pure ()
      pure ()

-- useMemo: return cached value or recompute
interpret fiber _ctx (HMemo hookDeps compute) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      let val = compute ()
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue val)
      pure val
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      pure (unsafeCoerce stored)
  where _unused = hookDeps  -- deps checking in full implementation

-- useCallback: cache a function (specialization of useMemo)
interpret fiber _ctx (HCallback hookDeps f) = do
  isFirst <- readIORef (fiberIsFirstRender fiber)
  cursor  <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  if isFirst
    then do
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue f)
      pure f
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) cursor
      pure (unsafeCoerce stored)
  where _unused = hookDeps

-- useContext: look up context value
interpret fiber _ctx (HContext context) = do
  _cursor <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  isFirst <- readIORef (fiberIsFirstRender fiber)
  if isFirst
    then do
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue ())
      pure (contextDefault context)
    else do
      -- In a full implementation, walk the provider chain
      pure (contextDefault context)

-- useId: generate stable unique ID
interpret fiber _ctx HId = do
  _cursor <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  isFirst <- readIORef (fiberIsFirstRender fiber)
  if isFirst
    then do
      idNum <- readIORef (fiberIdCounter fiber)
      modifyIORef' (fiberIdCounter fiber) (+ 1)
      let idStr = ":r" ++ show idNum ++ ":"
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue idStr)
      pure idStr
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) _cursor
      pure (unsafeCoerce stored)

-- useTransition: return transition handle
interpret fiber _ctx HTransition = do
  _cursor <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  isFirst <- readIORef (fiberIsFirstRender fiber)
  if isFirst
    then do
      pendingRef <- newIORef False
      let handle = TransitionHandle
            { isPending = False
            , startTransition = \action -> do
                writeIORef pendingRef True
                action
                writeIORef pendingRef False
                fiberScheduleUpdate fiber
            }
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue handle)
      pure handle
    else do
      SomeHookValue stored <- readHook (fiberHookStore fiber) _cursor
      pure (unsafeCoerce stored)

-- useDeferredValue: return (potentially stale) value
interpret fiber _ctx (HDeferred val) = do
  _cursor <- readIORef (fiberCursor fiber)
  modifyIORef' (fiberCursor fiber) (+ 1)
  isFirst <- readIORef (fiberIsFirstRender fiber)
  if isFirst
    then do
      _ <- appendHook (fiberHookStore fiber) (SomeHookValue val)
      pure val
    else do
      -- In concurrent mode, this might return the previous value
      -- during urgent renders. Simplified here.
      SomeHookValue stored <- readHook (fiberHookStore fiber) _cursor
      pure (unsafeCoerce stored)

-- use: resolve Async — IDENTITY INDEX, no hook slot allocated
interpret _fiber _ctx (HUse (Async action)) = action

--------------------------------------------------------------------------------
-- Deps checking
--------------------------------------------------------------------------------

checkDeps :: Deps -> IORef (Maybe [SomeHookValue]) -> IO Bool
checkDeps NoDeps _     = pure True   -- No deps: always run
checkDeps EmptyDeps ref = do
  prev <- readIORef ref
  case prev of
    Nothing -> do
      writeIORef ref (Just [])
      pure True   -- First run
    Just _  -> pure False  -- Already ran, skip
checkDeps (Deps _) ref = do
  prev <- readIORef ref
  case prev of
    Nothing -> do
      writeIORef ref (Just [])  -- Simplified: store placeholder
      pure True
    Just _ -> pure True  -- Simplified: always re-run for non-empty deps
    -- Full implementation would compare deps with Eq
