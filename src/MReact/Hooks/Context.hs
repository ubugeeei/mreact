{-# LANGUAGE DataKinds #-}

-- | @useContext@ — context subscription hook.
--
-- Allocates a @'SContext a@ slot and subscribes to the nearest
-- 'Context' provider in the component tree. Follows Rules of Hooks.
module MReact.Hooks.Context
  ( -- * useContext
    useContext
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useContext :: Context a -> Hooks i ('SContext a ': i) a
-- @
--
-- @
-- theme <- useContext themeContext
-- return $ div_ [style_ (themeStyles theme)] children
-- @
useContext :: ()
useContext = ()
