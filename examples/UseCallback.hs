{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useCallback example — demonstrates cached event handlers.
module UseCallback
  ( useCallbackApp
  ) where

import MReact.Prelude
import qualified Prelude

default (Int, String)

-- | A component that caches an event handler with useCallback.
--
-- @useCallback@ is a specialization of @useMemo@ for functions.
-- It returns a stable reference to the callback, preventing
-- unnecessary re-renders of child components that receive it as a prop.
useCallbackApp :: FC _ ()
useCallbackApp () = do
  (count, setCount) <- useState 0
  (label, setLabel) <- useState "Click me"

  -- This handler is cached and only recreated when `count` changes
  cachedHandler <- useCallback (deps count) (\(_ :: DOMEvent) ->
    setCount (count + 1) Prelude.>>
    Prelude.putStrLn ("Clicked! count was " ++ show count))

  return $ div []
    [ h1 [] ["useCallback Demo"]
    , p  [] [text ("Count: " ++ show count)]
    , p  [] [text ("Label: " ++ label)]
    , button [onClick cachedHandler] [text label]
    , button [onClick (\_ -> setLabel "Clicked!")] ["Change label"]
    ]
