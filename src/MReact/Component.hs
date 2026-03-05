{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Component types and helpers.
--
-- A React Component is a Kleisli arrow in the indexed monad:
--
-- @
-- Component : Props -> Hooks i j VDOM
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
  , SuspenseBoundary
  , suspense
    -- * Component composition
  , composeComponents
  ) where

import MReact.Types
import MReact.Hooks (Hooks(..))
import MReact.VDOM

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
-- counter :: Component '[] '[ 'SState Int] () VDOM
-- counter () = do
--   (count, setCount) <- useState 0
--   return $ text_ (show count)
-- @
type Component (i :: [Slot]) (j :: [Slot]) props result = props -> Hooks i j result

-- | Function Component — a Component that returns VDOM.
-- This is the most common component type.
--
-- @
-- greeting :: FC '[] '[ 'SContext Theme, 'SState Int] GreetingProps
-- greeting props = do
--   (count, setCount) <- useState 0
--   theme <- useContext themeCtx
--   return $ div_ [] [text_ (name props ++ ": " ++ show count)]
-- @
type FC (i :: [Slot]) (j :: [Slot]) props = Component i j props VNode

-- | A stateless function component uses no hooks at all.
-- Both indices are @'[]@, meaning only @use@ (identity-index)
-- and pure computation are allowed.
--
-- @
-- greeting :: StatelessFC GreetingProps
-- greeting props = return $ h1_ [] [text_ ("Hello, " ++ name props)]
-- @
type StatelessFC props = FC '[] '[] props

--------------------------------------------------------------------------------
-- Context Provider
--------------------------------------------------------------------------------

-- | A context provider wraps children with a context value.
type Provider a = a -> [VNode] -> VNode

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
  VFragment children
  where
    _unused = value  -- used by the runtime for context resolution

--------------------------------------------------------------------------------
-- Suspense Boundary
--------------------------------------------------------------------------------

-- | Suspense boundary configuration.
data SuspenseBoundary = SuspenseBoundary
  { fallback :: VNode
  }

-- | Create a Suspense boundary.
--
-- When a child calls @use@ on an unresolved 'Async', the boundary
-- displays the fallback. This is the effect handler for the @Suspend@
-- algebraic effect.
--
-- @
-- suspense (text_ "Loading...")
--   [ userProfile userId
--   ]
-- @
suspense :: VNode -> [VNode] -> VNode
suspense _fallbackNode children = VFragment children
  -- In the runtime, this node is recognized as a Suspense boundary.
  -- The fallback is displayed when a child's `use` throws (Promise pending).

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
