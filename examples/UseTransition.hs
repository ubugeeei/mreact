{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useTransition example — demonstrates non-urgent state updates.
module UseTransition
  ( useTransitionApp
  ) where

import MReact.Prelude

default (Int, String)

-- | A component that uses useTransition for non-urgent updates.
--
-- @useTransition@ returns a handle with:
--   * @isPending :: Bool@ — whether a transition is in progress
--   * @startTransition :: IO () -> IO ()@ — wrap an update to mark it non-urgent
--
-- Non-urgent updates can be interrupted by urgent ones (e.g., user input),
-- keeping the UI responsive during expensive state transitions.
useTransitionApp :: FC _ ()
useTransitionApp () = do
  (tab, setTab) <- useState "home"
  transition    <- useTransition

  return $ div []
    [ h1 [] ["useTransition Demo"]
    , nav []
        [ button [onClick (\_ -> startTransition transition (setTab "home"))]
            ["Home"]
        , button [onClick (\_ -> startTransition transition (setTab "about"))]
            ["About"]
        , button [onClick (\_ -> startTransition transition (setTab "contact"))]
            ["Contact"]
        ]
    , if isPending transition
        then p [class_ "loading"] ["Transitioning..."]
        else p [] [text ("Current tab: " ++ tab)]
    ]
