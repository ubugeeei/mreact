{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}

-- | Todo application — demonstrates useState, useReducer, useRef, useMemo, use.
module TodoApp
  ( todoApp
  , todoAppWithAsync
  , Todo(..)
  ) where

import MReact.Prelude
import qualified Prelude

default (Int, String)

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
-- The hook list @'[ SMemo Int, SRef String, SReducer [Todo]]@ is inferred.
-- Read right-to-left:
--   1. @SReducer [Todo]@ — useReducer for the todo list
--   2. @SRef String@ — useRef for the input value
--   3. @SMemo Int@ — useMemo for the completed count
todoApp :: FC _ ()
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

renderTodo :: Dispatch TodoAction -> Todo -> Fiber
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
--------------------------------------------------------------------------------

-- | Inner component that unwraps an Async with @use@.
todoList :: Async [Todo] -> Hooks i i Fiber
todoList todosPromise = do
  todos <- use todosPromise
  return $ ul []
    (Prelude.map (\t -> li [] [text (todoText t)]) todos)

-- | Container component that wraps the inner component in a Suspense boundary.
todoContainer :: Async [Todo] -> Hooks i i Fiber
todoContainer todosPromise =
  suspense (p [] [text "\x231B Downloading todos..."]) $
    todoList todosPromise

-- | Full app with a toggle button and Suspense boundary.
todoAppWithAsync :: Async [Todo] -> FC _ ()
todoAppWithAsync todosPromise () = do
  (showTodos, setShowTodos) <- useState True

  let content = if showTodos
        then todoContainer todosPromise
        else return $ div [] ["Todos hidden"]

  rendered <- content

  return $ div []
    [ button [onClick (\_ -> setShowTodos (not showTodos))]
        [text (if showTodos then "Hide" else "Show")]
    , rendered
    ]
