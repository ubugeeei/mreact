{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useRef example — demonstrates mutable refs that persist across renders.
module UseRef
  ( useRefApp
  ) where

import MReact.Prelude
import qualified Prelude

default (Int, String)

-- | A component that tracks render count using useRef.
--
-- Unlike useState, updating a ref does NOT trigger a re-render.
-- This makes refs ideal for:
--   * Storing previous values
--   * Tracking render counts
--   * Holding DOM element references
useRefApp :: FC _ ()
useRefApp () = do
  (count, setCount) <- useState 0
  renderCount       <- useRef 0

  -- Increment render count on every render (via useEffect)
  useEffect noDeps $
    readRef renderCount Prelude.>>= \n ->
    writeRef renderCount (n + 1)

  return $ div []
    [ h1 [] ["useRef Demo"]
    , p  [] [text ("Count: " ++ show count)]
    , p  [] [text "(Render count is tracked in a ref — no re-render on update)"]
    , button [onClick (\_ -> setCount (count + 1))] ["+"]
    ]
