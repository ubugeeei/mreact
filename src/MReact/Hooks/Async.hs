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
-- -- Valid: both branches are Hooks i i Fiber
-- if showDetails
--   then do
--     details <- use fetchDetails   -- Hooks i i String
--     return (text_ details)        -- Hooks i i Fiber
--   else
--     return nullElem               -- Hooks i i Fiber
-- @
--
-- == Suspension
--
-- When @use@ encounters a 'Pending' 'Async', the interpreter throws
-- a 'SuspendException'. This is caught by the nearest 'Suspense'
-- boundary (created via @suspense@), which renders the fallback
-- Fiber until the async resolves.
--
-- @
-- myComponent () = do
--   suspense (text "Loading...") $ do
--     userData <- use fetchUser      -- suspends if pending
--     return $ div [] [text (userName userData)]
-- @
--
-- == Contrast with useState
--
-- @useState@ adds @'SState s@ to the index and therefore __cannot__
-- appear conditionally:
--
-- @
-- -- Type error: 'SState Int ': i /= i
-- if cond
--   then do { (n, _) <- useState 0; ... }   -- Hooks i ('SState Int ': i) Fiber
--   else return nullElem                      -- Hooks i i Fiber
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
