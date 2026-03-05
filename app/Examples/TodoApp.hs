{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Todo application — demonstrates useState, useReducer, useRef, useMemo, use.
module Examples.TodoApp
  ( todoApp
  , todoAppWithAsync
  , Todo(..)
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

  return $ div [class_ "todo-app"]
    [ h1 [] ["Todo App"]
    , div [class_ "todo-input"]
        [ input [ placeholder "What needs to be done?"
                , onInput (\_ -> Prelude.pure ())
                ]
        , button [onClick (\_ ->
            readRef inputRef Prelude.>>= \val ->
            dispatch (AddTodo val) Prelude.>>
            writeRef inputRef ""
          )] ["Add"]
        ]
    , ul [class_ "todo-list"]
        (Prelude.map (renderTodo dispatch) todos)
    , footer [class_ "todo-footer"]
        [ text (show completedCount ++ " / " ++ show (Prelude.length todos) ++ " completed")
        ]
    ]

renderTodo :: Dispatch TodoAction -> Todo -> VNode
renderTodo dispatch todo =
  li [ class_ (if todoDone todo then "done" else "")
     , onClick (\_ -> dispatch (ToggleTodo (todoId todo)))
     ]
    [ span [] [text (todoText todo)]
    , button [onClick (\_ -> dispatch (RemoveTodo (todoId todo)))]
        ["x"]
    ]

--------------------------------------------------------------------------------
-- Async example: use + suspense
--
-- This demonstrates two key insights:
--   1. `use` has identity index, so it can appear inside if/else
--   2. `suspense` catches SuspendException and shows a fallback
--------------------------------------------------------------------------------

-- | A component that conditionally fetches data with `use` + `suspense`.
--
-- The type is @FC '[] '[ SState Bool] ()@ — note that `use` does NOT
-- appear in the type-level index, because its index is identity @'[]@.
-- This is what allows it inside the @if@ branch.
--
-- When @fetchTodos@ is still 'Pending', the @suspense@ boundary
-- catches the 'SuspendException' and renders "Loading..." instead.
-- Once resolved, a re-render is triggered and the todos are displayed.
todoAppWithAsync :: Async [Todo] -> FC '[] '[ SState Bool] ()
todoAppWithAsync fetchTodos () = do
  (showTodos, setShowTodos) <- useState True

  -- `use` inside control flow: VALID because index is identity
  -- `suspense` catches suspension and shows fallback
  let content = if showTodos
        then
          suspense (text "Loading todos...") $ do
            todos <- use fetchTodos  -- Hooks i i [Todo]  (identity index!)
            return $ ul []
              (Prelude.map (\t -> li [] [text (todoText t)]) todos)
        else
          return $ div [] ["Todos hidden"]

  rendered <- content

  return $ div []
    [ button [onClick (\_ -> setShowTodos (not showTodos))]
        [text (if showTodos then "Hide" else "Show")]
    , rendered
    ]
