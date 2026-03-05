{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Indexed (Atkey-style) monad type classes.
--
-- An indexed monad @m i j a@ tracks state transitions at the type level:
--
--   * @i@ — the state /before/ the computation
--   * @j@ — the state /after/ the computation
--   * @a@ — the result value
--
-- @ipure@ requires @i ~ j@ (no state change).
-- @ibind@ chains: @m i j a -> (a -> m j k b) -> m i k b@.
--
-- This is the foundation for enforcing Rules of Hooks:
-- each hook prepends a slot to the type-level state list,
-- making conditional hook usage a type error.
module MReact.Indexed
  ( -- * Indexed functor
    IxFunctor(..)
    -- * Indexed applicative
  , IxApplicative(..)
    -- * Indexed monad
  , IxMonad(..)
  ) where

-- | Indexed functor. Maps over the result while preserving state indices.
class IxFunctor (f :: k -> k -> * -> *) where
  imap :: (a -> b) -> f i j a -> f i j b

-- | Indexed applicative.
-- @ipure@ is the identity: no state change (@i ~ j@).
-- @iap@ sequences two indexed computations.
class IxFunctor f => IxApplicative (f :: k -> k -> * -> *) where
  ipure :: a -> f i i a
  iap   :: f i j (a -> b) -> f j l a -> f i l b

-- | Indexed monad (Atkey-style).
-- @ibind@ chains computations, composing state transitions.
class IxApplicative m => IxMonad (m :: k -> k -> * -> *) where
  ibind :: m i j a -> (a -> m j l b) -> m i l b
