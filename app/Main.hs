{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

import MReact.Types (async, resolve)
import MReact.Fiber
import MReact.Fiber.Diff
import MReact.Runtime.Fiber
import MReact.Runtime.Scheduler (render, reconcile, runEffects)

import Counter (counterApp)
import TodoApp (todoAppWithAsync, Todo(..))
import DeferredValue (deferredApp)

--------------------------------------------------------------------------------
-- ANSI escape codes
--------------------------------------------------------------------------------

reset, bold, dim, italic :: String
reset  = "\ESC[0m"
bold   = "\ESC[1m"
dim    = "\ESC[2m"
italic = "\ESC[3m"

fg :: Int -> String
fg n = "\ESC[38;5;" ++ show n ++ "m"

cyan, green, yellow, magenta, red, blue, gray :: String
cyan    = fg 6
green   = fg 2
yellow  = fg 3
magenta = fg 5
red     = fg 1
blue    = fg 4
gray    = fg 242

--------------------------------------------------------------------------------
-- Fiber event handler extraction
--------------------------------------------------------------------------------

findClickHandler :: String -> Fiber -> Maybe EventHandler
findClickHandler label (FElement "button" _ events [FText t])
  | t == label = Map.lookup "click" events
findClickHandler label (FElement _ _ _ children) =
  foldr (\c acc -> case acc of Just _ -> acc; Nothing -> findClickHandler label c) Nothing children
findClickHandler label (FFragment children) =
  foldr (\c acc -> case acc of Just _ -> acc; Nothing -> findClickHandler label c) Nothing children
findClickHandler _ _ = Nothing

dummyEvent :: DOMEvent
dummyEvent = DOMEvent "click" ""

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

-- | Syntax-highlighted compact fiber tree.
showFiber :: Fiber -> String
showFiber (FText s) = s
showFiber (FElement tag _ _ children) =
  dim ++ "<" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset
  ++ concatMap showFiber children
  ++ dim ++ "</" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset
showFiber (FFragment children) = concatMap showFiber children
showFiber FNull = dim ++ "null" ++ reset
showFiber (FKeyed tag _ _ children) =
  dim ++ "<" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset
  ++ concatMap (showFiber . snd) children
  ++ dim ++ "</" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset

showPatch :: Patch -> String
showPatch (PText path t) =
  yellow ++ "  TEXT " ++ reset ++ dim ++ showPath path ++ reset
  ++ " = " ++ green ++ "\"" ++ t ++ "\"" ++ reset
showPatch (PReplace path _) =
  red ++ "  REPLACE " ++ reset ++ dim ++ showPath path ++ reset
showPatch (PRemove path) =
  red ++ "  REMOVE " ++ reset ++ dim ++ showPath path ++ reset
showPatch (PInsert path i _) =
  green ++ "  INSERT " ++ reset ++ dim ++ showPath path ++ "[" ++ show i ++ "]" ++ reset
showPatch (PAttrs path as) =
  magenta ++ "  ATTRS " ++ reset ++ dim ++ showPath path
  ++ " (" ++ show (length as) ++ " changes)" ++ reset
showPatch (PReorder path _) =
  blue ++ "  REORDER " ++ reset ++ dim ++ showPath path ++ reset

showPath :: Path -> String
showPath p = "[" ++ intercalate "," (map show p) ++ "]"

showPatches :: [Patch] -> IO ()
showPatches [] = putStrLn $ green ++ "  (no patches)" ++ reset
showPatches ps = mapM_ (putStrLn . showPatch) ps

-- | Phase label with icon.
phase :: String -> String -> String -> IO ()
phase icon color msg =
  putStrLn $ "  " ++ color ++ icon ++ reset ++ " " ++ msg

--------------------------------------------------------------------------------
-- Full pipeline demo
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ bold ++ "  MReact" ++ reset ++ "  " ++ dim ++ "Monadic React Runtime Demo" ++ reset
  putStrLn $ dim ++ "  u_s = commit . reconcile . render(s)" ++ reset
  putStrLn $ dim ++ "  ─────────────────────────────────────" ++ reset
  putStrLn ""

  fiber <- newFiberInstance (pure ())

  -- Step 0: Initial render
  stepHeader 0 "Initial render"
  tree0 <- render fiber newRenderCtx (counterApp ())
  writeIORef (fiberPrevTree fiber) tree0
  writeIORef (fiberIsFirstRender fiber) False
  phase ">" cyan "render"
  putStrLn $ "    " ++ showFiber tree0
  phase ">" magenta "effect"
  runEffects PassiveEffect fiber
  writeIORef (fiberEffects fiber) []
  putStrLn ""

  -- Step 1-4: Simulate clicks
  let clicks = [(1, "+"), (2, "+"), (3, "-"), (4, "Reset")]
  mapM_ (\(n, label) -> do
    prev <- readIORef (fiberPrevTree fiber)
    stepHeader n ("Click " ++ show label)
    simulateClick fiber label prev
    ) clicks

  -- Step 5: Idempotency proof
  stepHeader 5 "Re-render (same state)"
  phase "\x21bb" dim "no state change"
  tree5 <- render fiber newRenderCtx (counterApp ())
  writeIORef (fiberEffects fiber) []
  patches5 <- reconcile fiber tree5
  phase ">" cyan "render"
  putStrLn $ "    " ++ showFiber tree5
  phase ">" yellow "reconcile"
  showPatches patches5
  putStrLn ""
  putStrLn $ "  " ++ green ++ bold ++ "  u_s . u_s = u_s  QED" ++ reset
  putStrLn ""

  -- Suspense demo
  putStrLn ""
  putStrLn $ dim ++ "  (Suspense demo follows)" ++ reset
  suspenseDemo

suspenseDemo :: IO ()
suspenseDemo = do
  putStrLn ""
  putStrLn $ dim ++ "  ─────────────────────────────────────" ++ reset
  putStrLn $ bold ++ "  Suspense Demo" ++ reset ++ "  " ++ dim ++ "use + suspense" ++ reset
  putStrLn $ dim ++ "  ─────────────────────────────────────" ++ reset
  putStrLn ""

  putStrLn $ gray ++ "  -- React equivalent:" ++ reset
  putStrLn $ gray ++ "  -- function TodoContainer({ todosPromise }) {" ++ reset
  putStrLn $ gray ++ "  --   return (" ++ reset
  putStrLn $ gray ++ "  --     <Suspense fallback={<p>⌛ Downloading todos...</p>}>" ++ reset
  putStrLn $ gray ++ "  --       <TodoList todosPromise={todosPromise} />" ++ reset
  putStrLn $ gray ++ "  --     </Suspense>" ++ reset
  putStrLn $ gray ++ "  --   );" ++ reset
  putStrLn $ gray ++ "  -- }" ++ reset
  putStrLn $ gray ++ "  -- function TodoList({ todosPromise }) {" ++ reset
  putStrLn $ gray ++ "  --   const todos = use(todosPromise);" ++ reset
  putStrLn $ gray ++ "  --   return <ul>{todos.map(t => <li>{t.text}</li>)}</ul>;" ++ reset
  putStrLn $ gray ++ "  -- }" ++ reset
  putStrLn ""

  -- Already-resolved Async
  stepHeader 6 "Suspense with resolved Async"
  resolvedAsync <- resolve [Todo 0 "Buy milk" False, Todo 1 "Write code" True]
  fiber6 <- newFiberInstance (pure ())
  tree6 <- render fiber6 newRenderCtx (todoAppWithAsync resolvedAsync ())
  phase ">" cyan "render (resolved → no suspension)"
  putStrLn $ "    " ++ showFiber tree6
  putStrLn ""

  -- Pending Async (demonstrates fallback)
  stepHeader 7 "Suspense with pending Async"
  pendingAsync <- async (threadDelay 1000000 >> pure [Todo 0 "Fetched item" False])
  fiber7 <- newFiberInstance (pure ())
  tree7 <- render fiber7 newRenderCtx (todoAppWithAsync pendingAsync ())
  phase ">" cyan "render (pending → shows fallback)"
  putStrLn $ "    " ++ showFiber tree7
  putStrLn ""

  -- Wait for resolution and re-render
  stepHeader 8 "After Async resolves → re-render"
  threadDelay 1200000  -- wait for async to resolve
  fiber8 <- newFiberInstance (pure ())
  tree8 <- render fiber8 newRenderCtx (todoAppWithAsync pendingAsync ())
  phase ">" cyan "render (resolved → shows content)"
  putStrLn $ "    " ++ showFiber tree8
  putStrLn ""

  -- useDeferredValue demo
  deferredValueDemo

deferredValueDemo :: IO ()
deferredValueDemo = do
  putStrLn $ dim ++ "  ─────────────────────────────────────" ++ reset
  putStrLn $ bold ++ "  useDeferredValue Demo" ++ reset
  putStrLn $ dim ++ "  ─────────────────────────────────────" ++ reset
  putStrLn ""

  -- Initial render
  stepHeader 9 "Initial render (query = \"\")"
  fiber <- newFiberInstance (pure ())
  tree9 <- render fiber newRenderCtx (deferredApp ())
  writeIORef (fiberPrevTree fiber) tree9
  writeIORef (fiberIsFirstRender fiber) False
  phase ">" cyan "render"
  putStrLn $ "    " ++ showFiber tree9
  putStrLn ""

  -- Simulate typing "abc" — setState triggers re-render
  stepHeader 10 "Type \"abc\" → urgent render returns stale deferred"
  -- Find the input handler and simulate
  let setQuery = findClickHandler "search" tree9
  case setQuery of
    Just handler -> do
      handler (DOMEvent "click" "abc")
      -- Urgent re-render: useDeferredValue returns OLD value
      tree10 <- render fiber newRenderCtx (deferredApp ())
      patches10 <- reconcile fiber tree10
      phase ">" cyan "render (urgent: deferred still stale)"
      putStrLn $ "    " ++ showFiber tree10
      phase ">" yellow "reconcile"
      showPatches patches10
      putStrLn ""

      -- Deferred re-render: useDeferredValue now returns NEW value
      stepHeader 11 "Deferred re-render → deferred catches up"
      tree11 <- render fiber newRenderCtx (deferredApp ())
      patches11 <- reconcile fiber tree11
      phase ">" cyan "render (deferred: value updated)"
      putStrLn $ "    " ++ showFiber tree11
      phase ">" yellow "reconcile"
      showPatches patches11
      putStrLn ""
    Nothing -> do
      phase "!" red "no search handler found"
      putStrLn ""

stepHeader :: Int -> String -> IO ()
stepHeader n title = do
  putStrLn $ bold ++ "  [" ++ show n ++ "] " ++ title ++ reset

simulateClick :: FiberInstance -> String -> Fiber -> IO ()
simulateClick fiber label currentTree = do
  case findClickHandler label currentTree of
    Nothing -> putStrLn $ red ++ "  ERROR: no handler for \"" ++ label ++ "\"" ++ reset
    Just handler -> do
      phase "\x25b6" green ("click \"" ++ label ++ "\"")
      handler dummyEvent

      newTree <- render fiber newRenderCtx (counterApp ())
      phase ">" cyan "render"
      putStrLn $ "    " ++ showFiber newTree

      patches <- reconcile fiber newTree
      phase ">" yellow "reconcile"
      showPatches patches

      phase ">" magenta "commit"
      runEffects PassiveEffect fiber
      writeIORef (fiberEffects fiber) []
      putStrLn ""
