{-# LANGUAGE DataKinds #-}

-- | @useTransition@ and @useDeferredValue@ — concurrent mode hooks.
--
-- Both allocate slots and follow Rules of Hooks.
--
-- == useDeferredValue
--
-- @useDeferredValue@ returns a "deferred" version of a value.
-- On the initial render, it returns the provided value as-is.
-- On subsequent renders, it first returns the OLD value (stale),
-- then schedules a background re-render where it commits the new value.
--
-- This is useful for keeping the UI responsive during expensive renders:
--
-- @
-- searchApp query () = do
--   (query, setQuery) <- useState ""
--   deferredQuery     <- useDeferredValue query
--   -- deferredQuery lags behind query during rapid updates,
--   -- allowing the input to remain responsive.
--   return $ div []
--     [ input [onInput (\e -> setQuery (eventTarget e))]
--     , searchResults deferredQuery
--     ]
-- @
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
-- -- deferredQuery may be stale during urgent renders
-- @
useDeferredValue :: ()
useDeferredValue = ()
