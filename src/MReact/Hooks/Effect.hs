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

    -- * useLayoutEffect
  , useLayoutEffect
  ) where

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useEffect :: Deps -> IO (IO ()) -> Hooks i ('SEffect ': i) ()
-- @
--
-- The @IO (IO ())@ follows React's pattern: the outer @IO@ is the
-- effect body, and the returned inner @IO ()@ is the cleanup function.
--
-- @
-- useEffect (deps count) $ do
--   putStrLn ("Count changed to: " ++ show count)
--   pure $ putStrLn "Cleaning up..."
-- @
useEffect :: ()
useEffect = ()

-- | See "MReact.Hooks" for the actual GADT constructor.
--
-- @
-- useLayoutEffect :: Deps -> IO (IO ()) -> Hooks i ('SLayoutEffect ': i) ()
-- @
useLayoutEffect :: ()
useLayoutEffect = ()
