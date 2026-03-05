{-# LANGUAGE DataKinds #-}

-- | @useTransition@ and @useDeferredValue@ — concurrent mode hooks.
--
-- Both allocate slots and follow Rules of Hooks.
module MReact.Hooks.Transition
  ( -- * useTransition
    useTransition

    -- * useDeferredValue
  , useDeferredValue
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useTransition :: Hooks i ('STransition ': i) TransitionHandle
-- @
--
-- @
-- th <- useTransition
-- let handleClick _ = startTransition th $ setItems newItems
-- @
useTransition :: ()
useTransition = ()

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useDeferredValue :: a -> Hooks i ('SDeferredValue a ': i) a
-- @
--
-- @
-- deferredQuery <- useDeferredValue query
-- @
useDeferredValue :: ()
useDeferredValue = ()
