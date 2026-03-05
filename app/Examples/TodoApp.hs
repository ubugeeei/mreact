{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

-- | Todo application — demonstrates useState, useReducer, useRef, useMemo, use.
module Examples.TodoApp
  ( todoApp
  , todoAppWithAsync
  ) where

import MReact.Prelude
import qualified Prelude

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Todo = Todo
  { todoId   :: Int
  , todoText :: String
  , todoDone :: Bool
  } deriving (Eq)

data TodoAction
  = AddTodo String
  | ToggleTodo Int
  | RemoveTodo Int

--------------------------------------------------------------------------------
-- Reducer
--------------------------------------------------------------------------------

todoReducer :: [Todo] -> TodoAction -> [Todo]
todoReducer todos (AddTodo txt) =
  let newId = if Prelude.null todos then 0 else Prelude.maximum (Prelude.map todoId todos) + 1
  in todos ++ [Todo newId txt False]
todoReducer todos (ToggleTodo tid) =
  Prelude.map (\t -> if todoId t == tid then t { todoDone = not (todoDone t) } else t) todos
todoReducer todos (RemoveTodo tid) =
  Prelude.filter (\t -> todoId t /= tid) todos

--------------------------------------------------------------------------------
-- Todo list component (useReducer + useRef + useMemo)
--------------------------------------------------------------------------------

-- | A todo app using useReducer for state management.
--
-- The type signature is:
-- @
-- FC '[] '[ SMemo Int, SRef String, SReducer [Todo]] ()
-- @
--
-- Read right-to-left:
--   1. @SReducer [Todo]@ — useReducer for the todo list
--   2. @SRef String@ — useRef for the input value
--   3. @SMemo Int@ — useMemo for the completed count
todoApp :: FC '[] '[ SMemo Int, SRef String, SReducer [Todo]] ()
todoApp () = do
  (todos, dispatch) <- useReducer todoReducer []
  inputRef <- useRef ""

  completedCount <- useMemo (deps todos) (\() ->
    Prelude.length (Prelude.filter todoDone todos))

  return $ div_ [class_ "todo-app"]
    [ h1_ [] [text_ "Todo App"]
    , div_ [class_ "todo-input"]
        [ input_ [ placeholder_ "What needs to be done?"
                  , onInput (\_ -> Prelude.pure ())  -- would update ref
                  ]
        , button_ [onClick (\_ ->
            readRef inputRef Prelude.>>= \val ->
            dispatch (AddTodo val) Prelude.>>
            writeRef inputRef ""
          )] [text_ "Add"]
        ]
    , ul_ [class_ "todo-list"]
        (Prelude.map (renderTodo dispatch) todos)
    , footer_ [class_ "todo-footer"]
        [ text_ (show completedCount ++ " / " ++ show (Prelude.length todos) ++ " completed")
        ]
    ]

renderTodo :: Dispatch TodoAction -> Todo -> VNode
renderTodo dispatch todo =
  li_ [ class_ (if todoDone todo then "done" else "")
       , onClick (\_ -> dispatch (ToggleTodo (todoId todo)))
       ]
    [ span_ [] [text_ (todoText todo)]
    , button_ [onClick (\_ -> dispatch (RemoveTodo (todoId todo)))]
        [text_ "x"]
    ]

--------------------------------------------------------------------------------
-- Async example: use inside control flow
--
-- This demonstrates the key insight: `use` has identity index,
-- so it can appear inside if/else without violating Rules of Hooks.
--------------------------------------------------------------------------------

-- | A component that conditionally fetches data with `use`.
--
-- The type is @FC '[] '[ SState Bool] ()@ — note that `use` does NOT
-- appear in the type-level index, because its index is identity @'[]@.
-- This is what allows it inside the @if@ branch.
todoAppWithAsync :: Async [Todo] -> FC '[] '[ SState Bool] ()
todoAppWithAsync fetchTodos () = do
  (showTodos, setShowTodos) <- useState True

  -- `use` inside control flow: VALID because index is identity
  let content = if showTodos
        then
          -- use fetchTodos :: Hooks i i [Todo]  (identity index!)
          let todosAsync = use fetchTodos
          -- In a real component, we'd bind this in do-notation.
          -- Showing the structure here:
          in div_ [] [text_ "Todos loaded"]
        else
          div_ [] [text_ "Todos hidden"]

  return $ div_ []
    [ button_ [onClick (\_ -> setShowTodos (not showTodos))]
        [text_ (if showTodos then "Hide" else "Show")]
    , content
    ]
