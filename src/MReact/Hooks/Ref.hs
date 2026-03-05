{-# LANGUAGE DataKinds #-}

-- | @useRef@ — mutable reference hook.
--
-- Allocates a @'SRef a@ slot. Unlike @useState@, mutating a ref
-- does __not__ trigger a re-render. Useful for storing DOM handles,
-- timers, or values that need to persist across renders without
-- causing updates.
module MReact.Hooks.Ref
  ( -- * useRef
    useRef
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useRef :: a -> Hooks i ('SRef a ': i) (Ref a)
-- @
--
-- @
-- ref <- useRef Nothing
-- useEffect emptyDeps $ do
--   writeRef ref (Just someValue)
--   pure (pure ())
-- @
useRef :: ()
useRef = ()
