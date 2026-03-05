{-# LANGUAGE DataKinds #-}

-- | @useId@ — stable unique identifier hook.
--
-- Allocates a @'SId@ slot. The generated ID is stable across
-- re-renders and unique within the application. Follows Rules of Hooks.
module MReact.Hooks.Id
  ( -- * useId
    useId
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useId :: Hooks i ('SId ': i) String
-- @
--
-- @
-- htmlId <- useId
-- return $ input_ [id_ htmlId, ariaLabelledBy_ (htmlId ++ "-label")] []
-- @
useId :: ()
useId = ()
