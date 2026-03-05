{-# LANGUAGE DataKinds #-}

-- | @useMemo@ and @useCallback@ — memoization hooks.
--
-- Both allocate slots ('SMemo' / 'SCallback') and follow Rules of Hooks.
-- @useCallback f deps@ is semantically equivalent to @useMemo deps (\() -> f)@.
module MReact.Hooks.Memo
  ( -- * useMemo
    useMemo

    -- * useCallback
  , useCallback
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useMemo :: Deps -> (() -> a) -> Hooks i ('SMemo a ': i) a
-- @
--
-- @
-- expensiveValue <- useMemo (deps items) (\() -> computeExpensive items)
-- @
useMemo :: ()
useMemo = ()

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useCallback :: Deps -> f -> Hooks i ('SCallback f ': i) f
-- @
--
-- @
-- handleClick <- useCallback (deps count) (\_ -> setCount (count + 1))
-- @
useCallback :: ()
useCallback = ()
