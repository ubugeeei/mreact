# mreact

**M**onadic **React** — React Hooks modeled as an indexed (graded) monad in Haskell.

> **Hobby project**

MReact encodes [Rules of Hooks](https://react.dev/reference/rules/rules-of-hooks) at the type level using Atkey-style indexed monads. Calling a hook like `useState` prepends a slot to a type-level list, so using it inside a conditional is a **compile-time error** — no linter needed.

The sole exception is `use` (React 19's Promise API), whose index is the monoid identity. This lets it appear inside `if`/`case`/loops, matching React's actual semantics.

## Quick start

```haskell
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

module MyComponent where

import MReact.Prelude

default (Int, String)

counter :: FC _ ()
counter () = do
  (count, setCount) <- useState 0

  let doubleCount   =  count * 2
  let parity        =  if even count then "even" else "odd"

  useEffect (deps count) $
    putStrLn $ "Count: " ++ show count

  return $ div [class_ "counter"]
    [ p  [] [text ("Count: " ++ show count)]
    , p  [] [text ("Double: " ++ show doubleCount)]
    , p  [] [text ("Parity: " ++ parity)]
    , button [onClick (\_ -> setCount (count + 1))] ["+"]
    ]
```

Import `MReact.Prelude` with `RebindableSyntax` and `OverloadedStrings` to get indexed do-notation and string-literal fibers. The hook list in `FC _ ()` is inferred via `PartialTypeSignatures` — no need to write it out. `default (Int, String)` lets GHC resolve numeric and string literal types automatically.

## How it works

### Indexed monad

```
Hooks :: [Slot] -> [Slot] -> * -> *
```

- `i` — hook state **before** the computation
- `j` — hook state **after**
- `a` — result value

Each hook prepends a `Slot` to `j`. `RebindableSyntax` rebinds `>>=` to `ibind`, which chains state transitions: `(i -> j) >>= (j -> k) = (i -> k)`.

### Rules of Hooks — enforced by types

```haskell
-- TYPE ERROR: branches have different indices
bad () = do
  if condition
    then do { (n, _) <- useState 0; ... }  -- Hooks i (SState Int ': i) Fiber
    else return nullElem                     -- Hooks i i Fiber
```

Both branches of an `if` must produce the same type. Since `useState` changes the index and `return` doesn't, GHC rejects this at compile time.

### `use` + `Suspense` — React 19's async data pattern

`use` unwraps an `Async` (Promise) with identity index, so it's safe inside control flow. When the `Async` is still pending, it throws a `SuspendException`, which the nearest `suspense` boundary catches to display a fallback.

This mirrors React 19's pattern directly:

```jsx
// React
function MessageContainer({ messagePromise }) {
  return (
    <Suspense fallback={<p>⌛Downloading message...</p>}>
      <Message messagePromise={messagePromise} />
    </Suspense>
  );
}
function Message({ messagePromise }) {
  const content = use(messagePromise);
  return <p>Here is the message: {content}</p>;
}
```

```haskell
-- MReact
message :: Async String -> Hooks i i Fiber
message messagePromise = do
  content <- use messagePromise
  return $ p [] [text ("Here is the message: " ++ content)]

messageContainer :: Async String -> Hooks i i Fiber
messageContainer messagePromise =
  suspense (p [] [text "⌛Downloading message..."]) $
    message messagePromise
```

Both `use` and `suspense` have identity index (`i -> i`), so they may appear inside conditionals and loops:

```haskell
if showDetails
  then do
    details <- use fetchDetails   -- Hooks i i String
    return (text details)         -- Hooks i i Fiber
  else
    return nullElem               -- Hooks i i Fiber
```

### `useDeferredValue` — stale-while-revalidate

`useDeferredValue` returns a "deferred" version of a value. On urgent re-renders, it returns the **old** value first, then schedules a background re-render to commit the new value:

```haskell
searchApp :: FC _ ()
searchApp () = do
  (query, setQuery) <- useState ""
  deferredQuery     <- useDeferredValue query
  -- Urgent render:   query = "abc", deferredQuery = ""  (stale)
  -- Deferred render: query = "abc", deferredQuery = "abc" (caught up)
  return $ div []
    [ input [onInput (\e -> setQuery (eventTarget e))]
    , p [] [text ("deferred: " ++ deferredQuery)]
    ]
```

### Derived state vs. `useMemo`

For cheap computations, use plain `let` bindings (derived state):

```haskell
let doubleCount = count * 2   -- recomputed every render, no hook slot
```

For expensive computations, use `useMemo` (allocates a `SMemo` slot):

```haskell
expensiveResult <- useMemo (deps items) (\() -> computeExpensive items)
```

## Supported hooks

| Hook | Slot | Index |
|---|---|---|
| `useState` | `SState s` | `i -> SState s ': i` |
| `useReducer` | `SReducer s` | `i -> SReducer s ': i` |
| `useRef` | `SRef a` | `i -> SRef a ': i` |
| `useEffect` | `SEffect` | `i -> SEffect ': i` |
| `useEffectWithCleanup` | `SEffect` | `i -> SEffect ': i` |
| `useLayoutEffect` | `SLayoutEffect` | `i -> SLayoutEffect ': i` |
| `useLayoutEffectWithCleanup` | `SLayoutEffect` | `i -> SLayoutEffect ': i` |
| `useMemo` | `SMemo a` | `i -> SMemo a ': i` |
| `useCallback` | `SCallback f` | `i -> SCallback f ': i` |
| `useContext` | `SContext a` | `i -> SContext a ': i` |
| `useId` | `SId` | `i -> SId ': i` |
| `useTransition` | `STransition` | `i -> STransition ': i` |
| `useDeferredValue` | `SDeferredValue a` | `i -> SDeferredValue a ': i` |
| `use` | *(none)* | `i -> i` (identity) |
| `suspense` | *(none)* | `i -> i` (identity) |

## Examples

Examples for each hook are in the `examples/` directory:

| File | Hooks demonstrated |
|---|---|
| `Counter.hs` | `useState`, `useEffect` |
| `TodoApp.hs` | `useReducer`, `useRef`, `useMemo`, `use`, `suspense` |
| `DeferredValue.hs` | `useDeferredValue`, `useState` |
| `UseRef.hs` | `useRef`, `useState`, `useEffect` |
| `UseMemo.hs` | `useMemo`, `useState` |
| `UseCallback.hs` | `useCallback`, `useState` |
| `UseContext.hs` | `useContext` |
| `UseId.hs` | `useId` |
| `UseTransition.hs` | `useTransition`, `useState` |

## Architecture

```
MReact.Indexed          -- IxFunctor, IxApplicative, IxMonad
MReact.Types            -- Slot, Deps, Ref, Async, Context, SuspendException
MReact.Hooks            -- Hooks GADT (free indexed monad)
MReact.Component        -- FC, StatelessFC, Provider, suspense
MReact.Fiber            -- Fiber node types
MReact.Fiber.Diff       -- Diffing algorithm (reconciliation)
MReact.DOM              -- HTML element DSL (JSX equivalent)
MReact.Runtime.Fiber    -- FiberInstance data structure (hook store)
MReact.Runtime.Scheduler -- Interpreter (algebraic effect handler)
MReact.Browser          -- GHCJS backend & SSR (renderToString)
MReact.Prelude          -- User-facing prelude (RebindableSyntax)
```

The rendering pipeline follows:

```
u_s = commit . reconcile . render(s)
```

with the idempotency property `u_s . u_s = u_s`.

## Setup

Requires [mise](https://mise.jdx.dev/) for environment management.

```bash
mise install        # installs ghcup + node
mise run build      # cabal build
```

GHC and Cabal are managed via ghcup. The project is tested with GHC 9.6.

### Dev server (SSR demo)

```bash
mise run dev        # build, generate index.html, serve on localhost:3000
```

This runs the Haskell SSR pipeline (`renderToString`) to generate `index.html`, then serves it locally.

### SSR only (no server)

```bash
mise run ssr        # prints SSR output to stdout
```

### GHCJS (browser, client-side)

```bash
cabal build -f ghcjs
```

## License

MIT
