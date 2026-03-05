{-# LANGUAGE DataKinds #-}

-- | @use@ — the Promise\/Async unwrapping API.
--
-- __This is the only hook-like API with identity index.__
--
-- @use : Async a -> Hooks i i a@
--
-- Because the index does not change (@i -> i@), @use@ can safely
-- appear inside @if@, @case@, guards, and loops. Both branches
-- of a conditional will have matching indices:
--
-- @
-- -- Valid: both branches are Hooks i i VDOM
-- if showDetails
--   then do
--     details <- use fetchDetails   -- Hooks i i String
--     return (text_ details)        -- Hooks i i VDOM
--   else
--     return nullElem               -- Hooks i i VDOM
-- @
--
-- Contrast with @useState@, which adds @'SState s@ to the index
-- and therefore __cannot__ appear conditionally:
--
-- @
-- -- Type error: 'SState Int ': i /= i
-- if cond
--   then do { (n, _) <- useState 0; ... }   -- Hooks i ('SState Int ': i) VDOM
--   else return nullElem                      -- Hooks i i VDOM
-- @
--
-- This encoding directly captures the insight from the discussion:
-- @use@ has the monoid identity as its grade, making it transparent
-- to the indexed monad's state tracking.
module MReact.Hooks.Async
  ( -- * use
    use
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor ('HUse').
use :: ()
use = ()
