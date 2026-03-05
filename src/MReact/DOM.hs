-- | HTML element DSL — the JSX equivalent for MReact.
--
-- Provides smart constructors for all common HTML elements,
-- designed to feel like JSX when used with @OverloadedStrings@:
--
-- @
-- div [class_ "app"]
--   [ h1 [] ["Hello"]
--   , button [onClick handleClick] ["Click me"]
--   ]
-- @
--
-- Elements follow the pattern: @tag :: [Prop] -> [VNode] -> VNode@
-- Void elements: @tag :: [Prop] -> VNode@
--
-- Props named @class_@, @type_@, and @id_@ keep the trailing underscore
-- to avoid conflicts with Haskell keywords or Prelude.
module MReact.DOM
  ( -- * Props
    Prop
  , toAttrsAndEvents
    -- * Attribute props
  , class_
  , id_
  , style
  , href
  , src
  , alt
  , type_
  , value
  , placeholder
  , disabled
  , checked
  , name
  , for
  , role
  , tabIndex
  , title
  , ariaLabel
  , ariaLabelledBy
  , dataAttr
  , attr
    -- * Event props
  , onClick
  , onDoubleClick
  , onChange
  , onInput
  , onSubmit
  , onKeyDown
  , onKeyUp
  , onKeyPress
  , onFocus
  , onBlur
  , onMouseEnter
  , onMouseLeave
  , onMouseDown
  , onMouseUp
  , onEvent
    -- * Key prop
  , key
    -- * Text
  , text
    -- * Layout elements
  , div
  , span
  , p
  , section
  , article
  , aside
  , header
  , footer
  , main
  , nav
    -- * Heading elements
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
    -- * Text elements
  , em
  , strong
  , code
  , pre
  , blockquote
    -- * List elements
  , ul
  , ol
  , li
    -- * Table elements
  , table
  , thead
  , tbody
  , tr
  , th
  , td
    -- * Form elements
  , form
  , label
  , input
  , textarea
  , select
  , option
  , button
    -- * Media elements
  , img
  , a
    -- * Fragment & null
  , fragment
  , nullElem
    -- * Keyed list helper
  , keyedList
  ) where

import Prelude hiding (div, span, id, head, map, pred)
import qualified Data.Map.Strict as Map
import MReact.VDOM

--------------------------------------------------------------------------------
-- Prop type
--------------------------------------------------------------------------------

-- | A prop is either an attribute or an event handler.
data Prop
  = AttrProp  !String !String
  | EventProp !String !EventHandler
  | KeyProp   !Key

-- | Partition props into attributes, event handlers, and optional key.
toAttrsAndEvents :: [Prop] -> (Attrs, Map.Map EventName EventHandler, Maybe Key)
toAttrsAndEvents = foldr go (Map.empty, Map.empty, Nothing)
  where
    go (AttrProp k v) (as, es, mk) = (Map.insert k v as, es, mk)
    go (EventProp n h) (as, es, mk) = (as, Map.insert n h es, mk)
    go (KeyProp k)    (as, es, _)  = (as, es, Just k)

--------------------------------------------------------------------------------
-- Attribute props
--------------------------------------------------------------------------------

class_ :: String -> Prop
class_ = AttrProp "class"

id_ :: String -> Prop
id_ = AttrProp "id"

style :: String -> Prop
style = AttrProp "style"

href :: String -> Prop
href = AttrProp "href"

src :: String -> Prop
src = AttrProp "src"

alt :: String -> Prop
alt = AttrProp "alt"

type_ :: String -> Prop
type_ = AttrProp "type"

value :: String -> Prop
value = AttrProp "value"

placeholder :: String -> Prop
placeholder = AttrProp "placeholder"

disabled :: Bool -> Prop
disabled True  = AttrProp "disabled" "disabled"
disabled False = AttrProp "disabled" ""

checked :: Bool -> Prop
checked True  = AttrProp "checked" "checked"
checked False = AttrProp "checked" ""

name :: String -> Prop
name = AttrProp "name"

for :: String -> Prop
for = AttrProp "for"

role :: String -> Prop
role = AttrProp "role"

tabIndex :: Int -> Prop
tabIndex = AttrProp "tabindex" . show

title :: String -> Prop
title = AttrProp "title"

ariaLabel :: String -> Prop
ariaLabel = AttrProp "aria-label"

ariaLabelledBy :: String -> Prop
ariaLabelledBy = AttrProp "aria-labelledby"

dataAttr :: String -> String -> Prop
dataAttr k = AttrProp ("data-" ++ k)

attr :: String -> String -> Prop
attr = AttrProp

--------------------------------------------------------------------------------
-- Event props
--------------------------------------------------------------------------------

onClick :: EventHandler -> Prop
onClick = EventProp "click"

onDoubleClick :: EventHandler -> Prop
onDoubleClick = EventProp "dblclick"

onChange :: EventHandler -> Prop
onChange = EventProp "change"

onInput :: EventHandler -> Prop
onInput = EventProp "input"

onSubmit :: EventHandler -> Prop
onSubmit = EventProp "submit"

onKeyDown :: EventHandler -> Prop
onKeyDown = EventProp "keydown"

onKeyUp :: EventHandler -> Prop
onKeyUp = EventProp "keyup"

onKeyPress :: EventHandler -> Prop
onKeyPress = EventProp "keypress"

onFocus :: EventHandler -> Prop
onFocus = EventProp "focus"

onBlur :: EventHandler -> Prop
onBlur = EventProp "blur"

onMouseEnter :: EventHandler -> Prop
onMouseEnter = EventProp "mouseenter"

onMouseLeave :: EventHandler -> Prop
onMouseLeave = EventProp "mouseleave"

onMouseDown :: EventHandler -> Prop
onMouseDown = EventProp "mousedown"

onMouseUp :: EventHandler -> Prop
onMouseUp = EventProp "mouseup"

onEvent :: EventName -> EventHandler -> Prop
onEvent = EventProp

-- | Key prop for list reconciliation.
key :: Key -> Prop
key = KeyProp

--------------------------------------------------------------------------------
-- Text helper
--------------------------------------------------------------------------------

-- | Create a text node from a dynamic string.
--
-- For string literals, use @OverloadedStrings@ instead:
--
-- @
-- h1 [] ["Hello"]                          -- string literal
-- p  [] [text ("Count: " ++ show count)]   -- dynamic string
-- @
text :: String -> VNode
text = VText

--------------------------------------------------------------------------------
-- Element helpers
--------------------------------------------------------------------------------

mkElement :: String -> [Prop] -> [VNode] -> VNode
mkElement tag props children =
  let (attrs, events, _mkey) = toAttrsAndEvents props
  in VElement tag attrs events children

mkVoid :: String -> [Prop] -> VNode
mkVoid tag props =
  let (attrs, events, _mkey) = toAttrsAndEvents props
  in VElement tag attrs events []

-- Layout
div :: [Prop] -> [VNode] -> VNode
div = mkElement "div"

span :: [Prop] -> [VNode] -> VNode
span = mkElement "span"

p :: [Prop] -> [VNode] -> VNode
p = mkElement "p"

section :: [Prop] -> [VNode] -> VNode
section = mkElement "section"

article :: [Prop] -> [VNode] -> VNode
article = mkElement "article"

aside :: [Prop] -> [VNode] -> VNode
aside = mkElement "aside"

header :: [Prop] -> [VNode] -> VNode
header = mkElement "header"

footer :: [Prop] -> [VNode] -> VNode
footer = mkElement "footer"

main :: [Prop] -> [VNode] -> VNode
main = mkElement "main"

nav :: [Prop] -> [VNode] -> VNode
nav = mkElement "nav"

-- Headings
h1 :: [Prop] -> [VNode] -> VNode
h1 = mkElement "h1"

h2 :: [Prop] -> [VNode] -> VNode
h2 = mkElement "h2"

h3 :: [Prop] -> [VNode] -> VNode
h3 = mkElement "h3"

h4 :: [Prop] -> [VNode] -> VNode
h4 = mkElement "h4"

h5 :: [Prop] -> [VNode] -> VNode
h5 = mkElement "h5"

h6 :: [Prop] -> [VNode] -> VNode
h6 = mkElement "h6"

-- Text
em :: [Prop] -> [VNode] -> VNode
em = mkElement "em"

strong :: [Prop] -> [VNode] -> VNode
strong = mkElement "strong"

code :: [Prop] -> [VNode] -> VNode
code = mkElement "code"

pre :: [Prop] -> [VNode] -> VNode
pre = mkElement "pre"

blockquote :: [Prop] -> [VNode] -> VNode
blockquote = mkElement "blockquote"

-- Lists
ul :: [Prop] -> [VNode] -> VNode
ul = mkElement "ul"

ol :: [Prop] -> [VNode] -> VNode
ol = mkElement "ol"

li :: [Prop] -> [VNode] -> VNode
li = mkElement "li"

-- Tables
table :: [Prop] -> [VNode] -> VNode
table = mkElement "table"

thead :: [Prop] -> [VNode] -> VNode
thead = mkElement "thead"

tbody :: [Prop] -> [VNode] -> VNode
tbody = mkElement "tbody"

tr :: [Prop] -> [VNode] -> VNode
tr = mkElement "tr"

th :: [Prop] -> [VNode] -> VNode
th = mkElement "th"

td :: [Prop] -> [VNode] -> VNode
td = mkElement "td"

-- Forms
form :: [Prop] -> [VNode] -> VNode
form = mkElement "form"

label :: [Prop] -> [VNode] -> VNode
label = mkElement "label"

input :: [Prop] -> VNode
input = mkVoid "input"

textarea :: [Prop] -> [VNode] -> VNode
textarea = mkElement "textarea"

select :: [Prop] -> [VNode] -> VNode
select = mkElement "select"

option :: [Prop] -> [VNode] -> VNode
option = mkElement "option"

button :: [Prop] -> [VNode] -> VNode
button = mkElement "button"

-- Media / links
img :: [Prop] -> VNode
img = mkVoid "img"

a :: [Prop] -> [VNode] -> VNode
a = mkElement "a"

-- Fragment & null
fragment :: [VNode] -> VNode
fragment = VFragment

nullElem :: VNode
nullElem = VNull

-- | Helper for rendering keyed lists (like mapping over arrays in JSX).
--
-- @
-- keyedList "ul" [] (map (\\item -> (itemId item, li [] [text (itemName item)])) items)
-- @
keyedList :: String -> [Prop] -> [(Key, VNode)] -> VNode
keyedList tag props items =
  let (attrs, events, _) = toAttrsAndEvents props
  in VKeyed tag attrs events items
