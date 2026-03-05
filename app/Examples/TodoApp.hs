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
-- Mirrors the React 19 pattern:
--
-- @
-- function MessageContainer({ messagePromise }) {
--   return (
--     <Suspense fallback={<p>⌛Downloading message...</p>}>
--       <Message messagePromise={messagePromise} />
--     </Suspense>
--   );
-- }
--
-- function Message({ messagePromise }) {
--   const content = use(messagePromise);
--   return <p>Here is the message: {content}</p>;
-- }
-- @
--------------------------------------------------------------------------------

-- | Inner component that unwraps an Async with @use@.
--
-- @use@ has identity index (@i -> i@), so @todoList@ is also identity.
-- This means it can appear inside control flow and Suspense boundaries.
todoList :: Async [Todo] -> Hooks i i VNode
todoList todosPromise = do
  todos <- use todosPromise
  return $ ul []
    (Prelude.map (\t -> li [] [text (todoText t)]) todos)

-- | Container component that wraps the inner component in a Suspense boundary.
--
-- Mirrors React's @\<Suspense fallback={...}\>@ pattern.
-- When @todosPromise@ is 'Pending', the fallback is shown.
-- When resolved, @todoList@ renders normally.
todoContainer :: Async [Todo] -> Hooks i i VNode
todoContainer todosPromise =
  suspense (p [] [text "\x231B Downloading todos..."]) $
    todoList todosPromise

-- | Full app with a toggle button and Suspense boundary.
--
-- The type is @FC '[] '[ SState Bool] ()@ — @use@ does NOT appear
-- in the index because it is identity. This is what allows it inside
-- the @if@ branch.
todoAppWithAsync :: Async [Todo] -> FC '[] '[ SState Bool] ()
todoAppWithAsync todosPromise () = do
  (showTodos, setShowTodos) <- useState True

  -- `use` inside control flow: VALID because index is identity
  -- `suspense` catches suspension and shows fallback
  let content = if showTodos
        then todoContainer todosPromise
        else return $ div [] ["Todos hidden"]

  rendered <- content

  return $ div []
    [ button [onClick (\_ -> setShowTodos (not showTodos))]
        [text (if showTodos then "Hide" else "Show")]
    , rendered
    ]
