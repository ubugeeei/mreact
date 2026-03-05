-- | MReact — React Hooks as an indexed monad in Haskell.
--
-- This library models React Hooks using Atkey-style indexed monads,
-- enforcing Rules of Hooks at the type level. The @use@ API has
-- identity index, allowing it inside control flow.
--
-- == Quick start
--
-- For writing components, import "MReact.Prelude" with @RebindableSyntax@:
--
-- @
-- {-\# LANGUAGE RebindableSyntax \#-}
-- {-\# LANGUAGE DataKinds \#-}
-- import MReact.Prelude
-- @
--
-- For library/runtime usage, import this module:
--
-- @
-- import MReact
-- @
--
-- == Module structure
--
-- * "MReact.Indexed" — @IxFunctor@, @IxApplicative@, @IxMonad@ type classes
-- * "MReact.Types" — Core types (@Slot@, @Deps@, @Ref@, @Async@, @Context@)
-- * "MReact.Hooks" — @Hooks@ GADT (the free indexed monad)
-- * "MReact.Hooks.State" — @useState@, @useReducer@ documentation
-- * "MReact.Hooks.Effect" — @useEffect@, @useLayoutEffect@ documentation
-- * "MReact.Hooks.Ref" — @useRef@ documentation
-- * "MReact.Hooks.Memo" — @useMemo@, @useCallback@ documentation
-- * "MReact.Hooks.Context" — @useContext@ documentation
-- * "MReact.Hooks.Async" — @use@ documentation
-- * "MReact.Hooks.Transition" — @useTransition@, @useDeferredValue@ documentation
-- * "MReact.Hooks.Id" — @useId@ documentation
-- * "MReact.Component" — @Component@, @FC@, @StatelessFC@ type aliases
-- * "MReact.Fiber" — Fiber node types
-- * "MReact.Fiber.Diff" — Fiber tree diffing algorithm
-- * "MReact.DOM" — HTML element DSL (JSX equivalent)
-- * "MReact.Runtime.Fiber" — FiberInstance data structure
-- * "MReact.Runtime.Scheduler" — Render pipeline & interpreter
-- * "MReact.Browser" — Browser backend (GHCJS) & SSR
module MReact
  ( -- * Indexed monad
    module MReact.Indexed
    -- * Types
  , module MReact.Types
    -- * Hooks
  , module MReact.Hooks
    -- * Components
  , module MReact.Component
    -- * Fiber
  , module MReact.Fiber
  , module MReact.Fiber.Diff
    -- * DOM DSL
  , module MReact.DOM
    -- * Runtime
  , module MReact.Runtime.Fiber
  , module MReact.Runtime.Scheduler
    -- * Browser
  , module MReact.Browser
  ) where

import MReact.Indexed
import MReact.Types
import MReact.Hooks
import MReact.Component
import MReact.Fiber
import MReact.Fiber.Diff
import MReact.DOM
import MReact.Runtime.Fiber
import MReact.Runtime.Scheduler
import MReact.Browser
