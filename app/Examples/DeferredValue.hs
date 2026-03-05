{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | useDeferredValue example — demonstrates deferred rendering.
--
-- Mirrors the React pattern:
--
-- @
-- function SearchApp() {
--   const [query, setQuery] = useState("");
--   const deferredQuery = useDeferredValue(query);
--   return (
--     <div>
--       <p>query: {query}</p>
--       <p>deferred: {deferredQuery}</p>
--       <button onClick={() => setQuery("abc")}>search</button>
--     </div>
--   );
-- }
-- @
module Examples.DeferredValue
  ( deferredApp
  ) where

import MReact.Prelude

-- | A search app demonstrating useDeferredValue.
--
-- When query changes (urgent update), deferredQuery still holds the
-- old value. A background re-render then commits the new value.
--
-- Type signature:
--   * @'SState String@ — useState for the query
--   * @'SDeferredValue String@ — useDeferredValue for deferred query
deferredApp :: FC '[] '[ SDeferredValue String, SState String] ()
deferredApp () = do
  (query, setQuery) <- useState ""
  deferredQuery     <- useDeferredValue query

  return $ div []
    [ p [] [text ("query: " ++ show query)]
    , p [] [text ("deferred: " ++ show deferredQuery)]
    , button [onClick (\_ -> setQuery "abc")] ["search"]
    ]
