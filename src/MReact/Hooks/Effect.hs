{-# LANGUAGE DataKinds #-}

-- | @useEffect@ and @useLayoutEffect@ — effect scheduling hooks.
--
-- Both allocate a slot in the type-level index ('SEffect' / 'SLayoutEffect'),
-- so they follow Rules of Hooks (cannot appear in conditionals).
--
-- The difference mirrors React:
--
--   * @useEffect@: runs asynchronously after the browser paints
--   * @useLayoutEffect@: runs synchronously after DOM mutations, before paint
module MReact.Hooks.Effect
  ( -- * useEffect
    useEffect
  , useEffectWithCleanup

    -- * useLayoutEffect
  , useLayoutEffect
  , useLayoutEffectWithCleanup
  ) where

-- | Schedule a side effect (runs after commit).
--
-- @
-- useEffect :: Deps -> IO () -> Hooks i ('SEffect ': i) ()
-- @
--
-- @
-- useEffect (deps count) $
--   putStrLn ("Count changed to: " ++ show count)
-- @
--
-- For effects that need a cleanup function, use 'useEffectWithCleanup'.
useEffect :: ()
useEffect = ()

-- | Like 'useEffect' but with a cleanup function.
--
-- @
-- useEffectWithCleanup :: Deps -> IO (IO ()) -> Hooks i ('SEffect ': i) ()
-- @
--
-- The @IO (IO ())@ follows React's pattern: the outer @IO@ is the
-- effect body, and the returned inner @IO ()@ is the cleanup function.
--
-- @
-- useEffectWithCleanup (deps count) $ do
--   putStrLn ("Count changed to: " ++ show count)
--   pure $ putStrLn "Cleaning up..."
-- @
useEffectWithCleanup :: ()
useEffectWithCleanup = ()

-- | Schedule a synchronous effect (runs before paint).
--
-- @
-- useLayoutEffect :: Deps -> IO () -> Hooks i ('SLayoutEffect ': i) ()
-- @
useLayoutEffect :: ()
useLayoutEffect = ()

-- | Like 'useLayoutEffect' but with a cleanup function.
useLayoutEffectWithCleanup :: ()
useLayoutEffectWithCleanup = ()
