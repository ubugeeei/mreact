{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | useContext example — demonstrates context subscription.
module UseContext
  ( useContextApp
  , themeCtx
  ) where

import MReact.Prelude

default (Int, String)

-- | Theme type for the context.
data Theme = Theme
  { themeName       :: String
  , themeBackground :: String
  , themeForeground :: String
  } deriving (Eq)

-- | A context with a default light theme.
themeCtx :: Context Theme
themeCtx = createContext Theme
  { themeName       = "light"
  , themeBackground = "#ffffff"
  , themeForeground = "#000000"
  }

-- | A component that reads the current theme from context.
--
-- @useContext@ subscribes to a context provider. When no provider
-- is found in the ancestor tree, the default value is used.
useContextApp :: FC _ ()
useContextApp () = do
  theme <- useContext themeCtx

  return $ div [ style ("background:" ++ themeBackground theme
                       ++ ";color:" ++ themeForeground theme) ]
    [ h1 [] ["useContext Demo"]
    , p  [] [text ("Current theme: " ++ themeName theme)]
    , p  [] [text ("Background: " ++ themeBackground theme)]
    , p  [] [text ("Foreground: " ++ themeForeground theme)]
    ]
