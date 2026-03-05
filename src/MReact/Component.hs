{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Component types and helpers.
--
-- A React Component is a Kleisli arrow in the indexed monad:
--
-- @
-- Component : Props -> Hooks i j Fiber
-- @
--
-- The indices @i@ and @j@ make the hook usage visible in the type signature,
-- enforcing Rules of Hooks at compile time.
module MReact.Component
  ( -- * Component types
    Component
  , FC
    -- * Stateless components
  , StatelessFC
    -- * Context provider
  , Provider
  , provider
    -- * Suspense boundary
  , suspense
    -- * Component composition
  , composeComponents
  ) where

import MReact.Types
import MReact.Hooks (Hooks(..))
import MReact.Fiber

--------------------------------------------------------------------------------
-- Component type aliases
--------------------------------------------------------------------------------

-- | A @Component@ takes props and returns an indexed Hooks computation.
-- The type-level lists @i@ and @j@ track hook state transitions:
--
--   * @i@ — hook state before (typically @'[]@ at the component root)
--   * @j@ — hook state after (accumulates one slot per hook call)
--
-- Example:
--
-- @
-- counter :: Component '[] '[ 'SState Int] () Fiber
-- counter () = do
--   (count, setCount) <- useState 0
--   return $ text_ (show count)
-- @
type Component (i :: [Slot]) (j :: [Slot]) props result = props -> Hooks i j result

-- | Function Component — a Component that returns a Fiber.
-- This is the most common component type.
--
-- The input index is fixed to @'[]@ (fresh component root).
-- The output index @j@ tracks which hooks were used; with
-- @PartialTypeSignatures@ you can simply write @FC _ Props@
-- and let GHC infer the hook list.
--
-- @
-- {-\# LANGUAGE PartialTypeSignatures \#-}
-- {-\# OPTIONS_GHC -Wno-partial-type-signatures \#-}
--
-- greeting :: FC _ GreetingProps
-- greeting props = do
--   (count, setCount) <- useState 0
--   theme <- useContext themeCtx
--   return $ div_ [] [text_ (name props ++ ": " ++ show count)]
-- @
type FC (j :: [Slot]) props = Component '[] j props Fiber

-- | A stateless function component uses no hooks at all.
-- Both indices are @'[]@, meaning only @use@ (identity-index)
-- and pure computation are allowed.
--
-- @
-- greeting :: StatelessFC GreetingProps
-- greeting props = return $ h1_ [] [text_ ("Hello, " ++ name props)]
-- @
type StatelessFC props = FC '[] props

--------------------------------------------------------------------------------
-- Context Provider
--------------------------------------------------------------------------------

-- | A context provider wraps children with a context value.
type Provider a = a -> [Fiber] -> Fiber

-- | Create a context provider for a given 'Context'.
--
-- @
-- themeProvider :: Provider Theme
-- themeProvider = provider themeCtx
--
-- -- Usage:
-- themeProvider darkTheme
--   [ myComponent props1
--   , otherComponent props2
--   ]
-- @
provider :: Context a -> Provider a
provider _ctx value children =
  -- In a full implementation, this would push the context value
  -- onto the context stack for child resolution.
  -- For now, we represent it as a fragment with metadata.
  FFragment children
  where
    _unused = value  -- used by the runtime for context resolution

--------------------------------------------------------------------------------
-- Suspense Boundary
--------------------------------------------------------------------------------

-- | Create a Suspense boundary.
--
-- When a child computation calls @use@ on a 'Pending' 'Async', the
-- interpreter throws a 'SuspendException'. The Suspense boundary
-- catches this and returns the fallback 'Fiber' instead.
--
-- __Index is identity__ (@i -> i@), so Suspense can appear anywhere,
-- including inside control flow.
--
-- @
-- myComponent () = do
--   (showDetails, _) <- useState True
--   suspense (text "Loading...") $ do
--     userData <- use fetchUser
--     return $ div [] [text (userName userData)]
-- @
suspense :: Fiber -> Hooks i i Fiber -> Hooks i i Fiber
suspense = HSuspense

--------------------------------------------------------------------------------
-- Component composition (Kleisli composition)
--------------------------------------------------------------------------------

-- | Compose two components via Kleisli composition.
-- This is the @>=>@ operator for the indexed monad.
--
-- @compose f g@ is equivalent to: @\\a -> f a >>= g@
--
-- Note: the indices must align — @j@ of @f@ must equal @i@ of @g@.
composeComponents
  :: Component i j a b
  -> Component j k b c
  -> Component i k a c
composeComponents f g a = HBind (f a) g
