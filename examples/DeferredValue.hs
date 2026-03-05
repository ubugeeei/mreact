{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useDeferredValue example — demonstrates deferred rendering.
module DeferredValue
  ( deferredApp
  ) where

import MReact.Prelude

default (Int, String)

-- | A search app demonstrating useDeferredValue.
--
-- When query changes (urgent update), deferredQuery still holds the
-- old value. A background re-render then commits the new value.
deferredApp :: FC _ ()
deferredApp () = do
  (query, setQuery) <- useState ""
  deferredQuery     <- useDeferredValue query

  return $ div []
    [ p [] [text ("query: " ++ show query)]
    , p [] [text ("deferred: " ++ show deferredQuery)]
    , button [onClick (\_ -> setQuery "abc")] ["search"]
    ]
