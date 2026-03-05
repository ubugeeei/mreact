{-# LANGUAGE DataKinds #-}

-- | @useState@ and @useReducer@ — stateful hooks that allocate a slot
-- in the type-level index.
module MReact.Hooks.State
  ( -- * useState
    useState

    -- * useReducer
  , useReducer
  ) where

-- Note: The actual constructors (HState, HReducer) live in MReact.Hooks.
-- These are smart constructors re-exported for user convenience.
-- They are defined in MReact.Hooks to avoid circular imports,
-- but documented here for conceptual grouping.

-- | Placeholder module — smart constructors are defined in "MReact.Hooks"
-- and re-exported through "MReact.Prelude".
--
-- @useState@ allocates a @'SState s@ slot:
--
-- @
-- useState :: s -> Hooks i ('SState s ': i) (s, SetState s)
-- @
--
-- The returned @SetState s@ takes the next value directly (State as a Snapshot):
--
-- @
-- (count, setCount) <- useState 0
-- -- ...
-- setCount (count + 1)   -- pass the next value, not an updater function
-- @
--
-- Using @useState@ inside a conditional is a __type error__:
--
-- @
-- -- Won't compile: branches have different indices
-- if cond
--   then do { (s, _) <- useState 0; ... }  -- Hooks i ('SState Int ': i) Fiber
--   else return nullElem                     -- Hooks i i Fiber
-- @
useState :: ()
useState = ()

-- | @useReducer@ allocates a @'SReducer s@ slot:
--
-- @
-- useReducer :: (s -> action -> s) -> s -> Hooks i ('SReducer s ': i) (s, Dispatch action)
-- @
useReducer :: ()
useReducer = ()
