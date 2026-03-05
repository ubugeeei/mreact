{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useId example — demonstrates stable unique identifier generation.
module UseId
  ( useIdApp
  ) where

import MReact.Prelude

default (Int, String)

-- | A component that generates stable unique IDs for accessibility.
--
-- @useId@ generates a unique ID that is stable across re-renders.
-- This is useful for associating labels with form inputs via
-- @for@/@id@ attributes, ensuring accessibility.
useIdApp :: FC _ ()
useIdApp () = do
  nameId  <- useId
  emailId <- useId

  return $ div []
    [ h1 [] ["useId Demo"]
    , div []
        [ label [for nameId]  ["Name: "]
        , input [id_ nameId, type_ "text", placeholder "Your name"]
        ]
    , div []
        [ label [for emailId] ["Email: "]
        , input [id_ emailId, type_ "email", placeholder "your@email.com"]
        ]
    , p [class_ "hint"] [text ("Generated IDs: " ++ nameId ++ ", " ++ emailId)]
    ]
