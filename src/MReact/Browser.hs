{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- | Browser backend for MReact.
--
-- When compiled with GHCJS (or the @ghcjs@ flag), this module uses
-- JavaScript FFI to manipulate the real DOM. When compiled with GHC,
-- it provides a logging/mock backend for testing.
--
-- Usage:
--
-- @
-- main :: IO ()
-- main = do
--   app <- mountApp "root" component props
--   eventLoop app
-- @
module MReact.Browser
  ( -- * App mounting
    mountApp
  , mountAppWith
    -- * Event loop
  , eventLoop
    -- * DOM operations (backend-specific)
  , DOMHandle(..)
  , createDOMHandle
  , applyPatchesToDOM
    -- * Server-side rendering
  , renderToString
  ) where

import Data.IORef
import qualified Data.Map.Strict as Map
import MReact.Hooks (Hooks)
import MReact.Types (Slot)
import MReact.VDOM
import MReact.VDOM.Diff
import MReact.Runtime.Fiber
import MReact.Runtime.Scheduler

--------------------------------------------------------------------------------
-- DOM Handle
--------------------------------------------------------------------------------

-- | Handle to the real DOM root element.
data DOMHandle = DOMHandle
  { domRootId   :: !String
  , domVDOM     :: !(IORef VNode)   -- ^ Current VDOM state
#ifdef GHCJS
  , domElement  :: !JSVal           -- ^ JS DOM element reference
#endif
  }

-- | Create a handle to a DOM root element by its ID.
createDOMHandle :: String -> IO DOMHandle
createDOMHandle rootId = do
  vdomRef <- newIORef VNull
#ifdef GHCJS
  el <- js_getElementById rootId
  pure (DOMHandle rootId vdomRef el)
#else
  pure (DOMHandle rootId vdomRef)
#endif

--------------------------------------------------------------------------------
-- Mounting
--------------------------------------------------------------------------------

-- | Mount a component onto a DOM element by ID.
--
-- @
-- main = mountApp "root" myComponent myProps
-- @
mountApp :: String                   -- ^ DOM element ID
         -> (props -> Hooks '[] j VNode)  -- ^ Component
         -> props                    -- ^ Initial props
         -> IO MountedApp
mountApp rootId component props = do
  domHandle <- createDOMHandle rootId
  mountAppWith domHandle component props

-- | Mount with an explicit DOM handle.
mountAppWith :: DOMHandle
             -> (props -> Hooks '[] j VNode)
             -> props
             -> IO MountedApp
mountAppWith domHandle component props = do
  let commitFn = applyPatchesToDOM domHandle
  mount commitFn newRenderCtx (component props)

--------------------------------------------------------------------------------
-- Patch application
--------------------------------------------------------------------------------

-- | Apply patches to the real DOM.
applyPatchesToDOM :: DOMHandle -> [Patch] -> IO ()
applyPatchesToDOM _handle [] = pure ()  -- No patches = no-op (idempotency!)
applyPatchesToDOM handle patches = do
#ifdef GHCJS
  mapM_ (applyPatchJS (domElement handle)) patches
#else
  -- GHC backend: log patches for testing/debugging
  putStrLn $ "[MReact] Applying " ++ show (length patches) ++ " patches:"
  mapM_ (putStrLn . ("  " ++) . showPatch) patches
  -- Update internal VDOM state
  modifyIORef' (domVDOM handle) (applyPatchesVDOM patches)
#endif

-- | Pretty-print a patch for the logging backend.
showPatch :: Patch -> String
showPatch (PReplace path _)    = "REPLACE at " ++ show path
showPatch (PRemove path)       = "REMOVE at " ++ show path
showPatch (PInsert path i _)   = "INSERT at " ++ show path ++ "[" ++ show i ++ "]"
showPatch (PText path t)       = "TEXT at " ++ show path ++ " = " ++ show t
showPatch (PAttrs path as)     = "ATTRS at " ++ show path ++ " (" ++ show (length as) ++ " changes)"
showPatch (PReorder path _)    = "REORDER at " ++ show path

-- | Apply patches to a VDOM tree (for the GHC testing backend).
applyPatchesVDOM :: [Patch] -> VNode -> VNode
applyPatchesVDOM [] vdom = vdom
applyPatchesVDOM (p:ps) vdom = applyPatchesVDOM ps (applyOneVDOM p vdom)

applyOneVDOM :: Patch -> VNode -> VNode
applyOneVDOM (PReplace [] new)     _    = new
applyOneVDOM (PText [] t)          _    = VText t
applyOneVDOM (PAttrs [] changes) (VElement tag attrs evts children) =
  VElement tag (foldl applyAttrPatch attrs changes) evts children
applyOneVDOM (PReplace (i:rest) new) (VElement tag attrs evts children) =
  VElement tag attrs evts (modifyChild i (applyOneVDOM (PReplace rest new)) children)
applyOneVDOM (PText (i:rest) t) (VElement tag attrs evts children) =
  VElement tag attrs evts (modifyChild i (applyOneVDOM (PText rest t)) children)
applyOneVDOM _ vdom = vdom  -- Simplified: other cases

applyAttrPatch :: Attrs -> AttrPatch -> Attrs
applyAttrPatch attrs (SetAttr k v)  = Map.insert k v attrs
applyAttrPatch attrs (RemoveAttr k) = Map.delete k attrs

modifyChild :: Int -> (VNode -> VNode) -> [VNode] -> [VNode]
modifyChild _ _ [] = []
modifyChild 0 f (x:xs) = f x : xs
modifyChild n f (x:xs) = x : modifyChild (n-1) f xs

--------------------------------------------------------------------------------
-- Event loop
--------------------------------------------------------------------------------

-- | Simple event loop for the GHC backend.
-- In GHCJS, the browser's event loop handles this automatically.
eventLoop :: MountedApp -> IO ()
eventLoop _app = do
#ifdef GHCJS
  -- GHCJS: browser event loop handles everything
  pure ()
#else
  putStrLn "[MReact] App mounted. (GHC backend — no event loop)"
  putStrLn "[MReact] Use GHCJS to run in browser."
#endif

--------------------------------------------------------------------------------
-- Server-side rendering (SSR)
--------------------------------------------------------------------------------

-- | Render a component to an HTML string (for SSR).
--
-- This is the server effect handler:
-- @handleServer : Eff_React(VDOM) -> HTML string@
renderToString :: Hooks '[] j VNode -> IO String
renderToString component = do
  fiber <- newFiber (pure ())
  vdom <- interpret fiber newRenderCtx component
  pure (vdomToHTML vdom)

-- | Convert VDOM to HTML string.
vdomToHTML :: VNode -> String
vdomToHTML (VText s) = escapeHTML s
vdomToHTML (VElement tag attrs _ children) =
  "<" ++ tag ++ renderAttrs attrs ++ ">"
  ++ concatMap vdomToHTML children
  ++ "</" ++ tag ++ ">"
vdomToHTML (VKeyed tag attrs _ children) =
  "<" ++ tag ++ renderAttrs attrs ++ ">"
  ++ concatMap (vdomToHTML . snd) children
  ++ "</" ++ tag ++ ">"
vdomToHTML (VFragment children) = concatMap vdomToHTML children
vdomToHTML VNull = ""

renderAttrs :: Attrs -> String
renderAttrs attrs
  | Map.null attrs = ""
  | otherwise = concatMap renderAttr (Map.toList attrs)
  where
    renderAttr (k, v) = " " ++ k ++ "=\"" ++ escapeHTML v ++ "\""

escapeHTML :: String -> String
escapeHTML = concatMap esc
  where
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '&'  = "&amp;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c    = [c]

--------------------------------------------------------------------------------
-- GHCJS FFI (only when compiled with GHCJS)
--------------------------------------------------------------------------------

#ifdef GHCJS
foreign import javascript unsafe
  "document.getElementById($1)"
  js_getElementById :: String -> IO JSVal

foreign import javascript unsafe
  "document.createElement($1)"
  js_createElement :: String -> IO JSVal

foreign import javascript unsafe
  "document.createTextNode($1)"
  js_createTextNode :: String -> IO JSVal

foreign import javascript unsafe
  "$1.appendChild($2)"
  js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.removeChild($2)"
  js_removeChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  js_replaceChild :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  js_setAttribute :: JSVal -> String -> String -> IO ()

foreign import javascript unsafe
  "$1.removeAttribute($2)"
  js_removeAttribute :: JSVal -> String -> IO ()

foreign import javascript unsafe
  "$1.textContent = $2"
  js_setTextContent :: JSVal -> String -> IO ()

foreign import javascript unsafe
  "$1.childNodes[$2]"
  js_childAt :: JSVal -> Int -> IO JSVal

foreign import javascript unsafe
  "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> String -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1.innerHTML = ''"
  js_clearElement :: JSVal -> IO ()

-- | Apply a single patch using GHCJS FFI.
applyPatchJS :: JSVal -> Patch -> IO ()
applyPatchJS root (PReplace path new) = do
  parent <- navigateToParent root path
  let idx = last path
  oldChild <- js_childAt parent idx
  newEl <- vdomToDOM new
  js_replaceChild parent newEl oldChild

applyPatchJS root (PText path txt) = do
  el <- navigateTo root path
  js_setTextContent el txt

applyPatchJS root (PAttrs path changes) = do
  el <- navigateTo root path
  mapM_ (applyAttrPatchJS el) changes

applyPatchJS root (PRemove path) = do
  parent <- navigateToParent root path
  let idx = last path
  child <- js_childAt parent idx
  js_removeChild parent child

applyPatchJS root (PInsert path idx new) = do
  parent <- navigateTo root path
  newEl <- vdomToDOM new
  js_appendChild parent newEl

applyPatchJS _ _ = pure ()

applyAttrPatchJS :: JSVal -> AttrPatch -> IO ()
applyAttrPatchJS el (SetAttr k v) = js_setAttribute el k v
applyAttrPatchJS el (RemoveAttr k) = js_removeAttribute el k

navigateTo :: JSVal -> Path -> IO JSVal
navigateTo el []     = pure el
navigateTo el (i:is) = do
  child <- js_childAt el i
  navigateTo child is

navigateToParent :: JSVal -> Path -> IO JSVal
navigateToParent el path = navigateTo el (init path)

-- | Convert a VNode to a real DOM element.
vdomToDOM :: VNode -> IO JSVal
vdomToDOM (VText s) = js_createTextNode s
vdomToDOM (VElement tag attrs events children) = do
  el <- js_createElement tag
  mapM_ (\(k,v) -> js_setAttribute el k v) (Map.toList attrs)
  -- Event binding would go here
  mapM_ (\child -> vdomToDOM child >>= js_appendChild el) children
  pure el
vdomToDOM (VFragment children) = do
  frag <- js_createElement "div"  -- Simplified
  mapM_ (\child -> vdomToDOM child >>= js_appendChild frag) children
  pure frag
vdomToDOM VNull = js_createTextNode ""
vdomToDOM (VKeyed tag attrs events children) =
  vdomToDOM (VElement tag attrs events (map snd children))
#endif
