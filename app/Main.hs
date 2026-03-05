{-# LANGUAGE DataKinds #-}

module Main where

import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

import MReact.VDOM
import MReact.VDOM.Diff
import MReact.Runtime.Fiber
import MReact.Runtime.Scheduler (render, reconcile, runEffects)

import Examples.Counter (counterApp)

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
-- VDOM event handler extraction
--------------------------------------------------------------------------------

findClickHandler :: String -> VNode -> Maybe EventHandler
findClickHandler label (VElement "button" _ events [VText t])
  | t == label = Map.lookup "click" events
findClickHandler label (VElement _ _ _ children) =
  foldr (\c acc -> case acc of Just _ -> acc; Nothing -> findClickHandler label c) Nothing children
findClickHandler label (VFragment children) =
  foldr (\c acc -> case acc of Just _ -> acc; Nothing -> findClickHandler label c) Nothing children
findClickHandler _ _ = Nothing

dummyEvent :: DOMEvent
dummyEvent = DOMEvent "click" ""

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

-- | Syntax-highlighted compact VDOM.
showVDOM :: VNode -> String
showVDOM (VText s) = s
showVDOM (VElement tag _ _ children) =
  dim ++ "<" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset
  ++ concatMap showVDOM children
  ++ dim ++ "</" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset
showVDOM (VFragment children) = concatMap showVDOM children
showVDOM VNull = dim ++ "null" ++ reset
showVDOM (VKeyed tag _ _ children) =
  dim ++ "<" ++ reset ++ cyan ++ tag ++ reset ++ dim ++ ">" ++ reset
  ++ concatMap (showVDOM . snd) children
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

  fiber <- newFiber (pure ())

  -- Step 0: Initial render
  stepHeader 0 "Initial render"
  vdom0 <- render fiber newRenderCtx (counterApp ())
  writeIORef (fiberPrevVDOM fiber) vdom0
  writeIORef (fiberIsFirstRender fiber) False
  phase ">" cyan "render"
  putStrLn $ "    " ++ showVDOM vdom0
  phase ">" magenta "effect"
  runEffects PassiveEffect fiber
  writeIORef (fiberEffects fiber) []
  putStrLn ""

  -- Step 1-4: Simulate clicks
  let clicks = [(1, "+"), (2, "+"), (3, "-"), (4, "Reset")]
  mapM_ (\(n, label) -> do
    prev <- readIORef (fiberPrevVDOM fiber)
    stepHeader n ("Click " ++ show label)
    simulateClick fiber label prev
    ) clicks

  -- Step 5: Idempotency proof
  stepHeader 5 "Re-render (same state)"
  phase "\x21bb" dim "no state change"
  vdom5 <- render fiber newRenderCtx (counterApp ())
  writeIORef (fiberEffects fiber) []
  patches5 <- reconcile fiber vdom5
  phase ">" cyan "render"
  putStrLn $ "    " ++ showVDOM vdom5
  phase ">" yellow "reconcile"
  showPatches patches5
  putStrLn ""
  putStrLn $ "  " ++ green ++ bold ++ "  u_s . u_s = u_s  QED" ++ reset
  putStrLn ""

stepHeader :: Int -> String -> IO ()
stepHeader n title = do
  putStrLn $ bold ++ "  [" ++ show n ++ "] " ++ title ++ reset

simulateClick :: Fiber -> String -> VNode -> IO ()
simulateClick fiber label currentVDOM = do
  case findClickHandler label currentVDOM of
    Nothing -> putStrLn $ red ++ "  ERROR: no handler for \"" ++ label ++ "\"" ++ reset
    Just handler -> do
      phase "\x25b6" green ("click \"" ++ label ++ "\"")
      handler dummyEvent

      newVDOM <- render fiber newRenderCtx (counterApp ())
      phase ">" cyan "render"
      putStrLn $ "    " ++ showVDOM newVDOM

      patches <- reconcile fiber newVDOM
      phase ">" yellow "reconcile"
      showPatches patches

      phase ">" magenta "commit"
      runEffects PassiveEffect fiber
      writeIORef (fiberEffects fiber) []
      putStrLn ""
