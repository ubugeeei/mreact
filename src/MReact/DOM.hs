-- | HTML element DSL — the JSX equivalent for MReact.
--
-- Provides smart constructors for all common HTML elements,
-- designed to feel like JSX when used with @RebindableSyntax@:
--
-- @
-- -- JSX:
-- -- \<div className="app"\>
-- --   \<h1\>Hello\<\/h1\>
-- --   \<button onClick={handleClick}\>Click me\<\/button\>
-- -- \<\/div\>
--
-- -- MReact:
-- div_ [class_ "app"]
--   [ h1_ [] [text_ "Hello"]
--   , button_ [onClick handleClick] [text_ "Click me"]
--   ]
-- @
--
-- Elements follow the pattern: @tag_ :: [Prop] -> [VNode] -> VNode@
-- Void elements: @tag_ :: [Prop] -> VNode@
module MReact.DOM
  ( -- * Props
    Prop
  , toAttrsAndEvents
    -- * Attribute props
  , class_
  , id_
  , style_
  , href_
  , src_
  , alt_
  , type_
  , value_
  , placeholder_
  , disabled_
  , checked_
  , name_
  , for_
  , role_
  , tabIndex_
  , title_
  , ariaLabel_
  , ariaLabelledBy_
  , dataAttr_
  , attr_
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
  , key_
    -- * Layout elements
  , div_
  , span_
  , p_
  , section_
  , article_
  , aside_
  , header_
  , footer_
  , main_
  , nav_
    -- * Heading elements
  , h1_
  , h2_
  , h3_
  , h4_
  , h5_
  , h6_
    -- * Text elements
  , text_
  , em_
  , strong_
  , code_
  , pre_
  , blockquote_
    -- * List elements
  , ul_
  , ol_
  , li_
    -- * Table elements
  , table_
  , thead_
  , tbody_
  , tr_
  , th_
  , td_
    -- * Form elements
  , form_
  , label_
  , input_
  , textarea_
  , select_
  , option_
  , button_
    -- * Media elements
  , img_
  , a_
    -- * Fragment & null
  , fragment_
  , nullElem
    -- * Keyed list helper
  , keyedList_
  ) where

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

style_ :: String -> Prop
style_ = AttrProp "style"

href_ :: String -> Prop
href_ = AttrProp "href"

src_ :: String -> Prop
src_ = AttrProp "src"

alt_ :: String -> Prop
alt_ = AttrProp "alt"

type_ :: String -> Prop
type_ = AttrProp "type"

value_ :: String -> Prop
value_ = AttrProp "value"

placeholder_ :: String -> Prop
placeholder_ = AttrProp "placeholder"

disabled_ :: Bool -> Prop
disabled_ True  = AttrProp "disabled" "disabled"
disabled_ False = AttrProp "disabled" ""

checked_ :: Bool -> Prop
checked_ True  = AttrProp "checked" "checked"
checked_ False = AttrProp "checked" ""

name_ :: String -> Prop
name_ = AttrProp "name"

for_ :: String -> Prop
for_ = AttrProp "for"

role_ :: String -> Prop
role_ = AttrProp "role"

tabIndex_ :: Int -> Prop
tabIndex_ = AttrProp "tabindex" . show

title_ :: String -> Prop
title_ = AttrProp "title"

ariaLabel_ :: String -> Prop
ariaLabel_ = AttrProp "aria-label"

ariaLabelledBy_ :: String -> Prop
ariaLabelledBy_ = AttrProp "aria-labelledby"

dataAttr_ :: String -> String -> Prop
dataAttr_ k = AttrProp ("data-" ++ k)

attr_ :: String -> String -> Prop
attr_ = AttrProp

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
key_ :: Key -> Prop
key_ = KeyProp

--------------------------------------------------------------------------------
-- Element helpers
--
-- Pattern: tag_ :: [Prop] -> [VNode] -> VNode
-- The [Prop] list is partitioned into attributes and event handlers.
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
div_ :: [Prop] -> [VNode] -> VNode
div_ = mkElement "div"

span_ :: [Prop] -> [VNode] -> VNode
span_ = mkElement "span"

p_ :: [Prop] -> [VNode] -> VNode
p_ = mkElement "p"

section_ :: [Prop] -> [VNode] -> VNode
section_ = mkElement "section"

article_ :: [Prop] -> [VNode] -> VNode
article_ = mkElement "article"

aside_ :: [Prop] -> [VNode] -> VNode
aside_ = mkElement "aside"

header_ :: [Prop] -> [VNode] -> VNode
header_ = mkElement "header"

footer_ :: [Prop] -> [VNode] -> VNode
footer_ = mkElement "footer"

main_ :: [Prop] -> [VNode] -> VNode
main_ = mkElement "main"

nav_ :: [Prop] -> [VNode] -> VNode
nav_ = mkElement "nav"

-- Headings
h1_ :: [Prop] -> [VNode] -> VNode
h1_ = mkElement "h1"

h2_ :: [Prop] -> [VNode] -> VNode
h2_ = mkElement "h2"

h3_ :: [Prop] -> [VNode] -> VNode
h3_ = mkElement "h3"

h4_ :: [Prop] -> [VNode] -> VNode
h4_ = mkElement "h4"

h5_ :: [Prop] -> [VNode] -> VNode
h5_ = mkElement "h5"

h6_ :: [Prop] -> [VNode] -> VNode
h6_ = mkElement "h6"

-- Text
text_ :: String -> VNode
text_ = VText

em_ :: [Prop] -> [VNode] -> VNode
em_ = mkElement "em"

strong_ :: [Prop] -> [VNode] -> VNode
strong_ = mkElement "strong"

code_ :: [Prop] -> [VNode] -> VNode
code_ = mkElement "code"

pre_ :: [Prop] -> [VNode] -> VNode
pre_ = mkElement "pre"

blockquote_ :: [Prop] -> [VNode] -> VNode
blockquote_ = mkElement "blockquote"

-- Lists
ul_ :: [Prop] -> [VNode] -> VNode
ul_ = mkElement "ul"

ol_ :: [Prop] -> [VNode] -> VNode
ol_ = mkElement "ol"

li_ :: [Prop] -> [VNode] -> VNode
li_ = mkElement "li"

-- Tables
table_ :: [Prop] -> [VNode] -> VNode
table_ = mkElement "table"

thead_ :: [Prop] -> [VNode] -> VNode
thead_ = mkElement "thead"

tbody_ :: [Prop] -> [VNode] -> VNode
tbody_ = mkElement "tbody"

tr_ :: [Prop] -> [VNode] -> VNode
tr_ = mkElement "tr"

th_ :: [Prop] -> [VNode] -> VNode
th_ = mkElement "th"

td_ :: [Prop] -> [VNode] -> VNode
td_ = mkElement "td"

-- Forms
form_ :: [Prop] -> [VNode] -> VNode
form_ = mkElement "form"

label_ :: [Prop] -> [VNode] -> VNode
label_ = mkElement "label"

input_ :: [Prop] -> VNode
input_ = mkVoid "input"

textarea_ :: [Prop] -> [VNode] -> VNode
textarea_ = mkElement "textarea"

select_ :: [Prop] -> [VNode] -> VNode
select_ = mkElement "select"

option_ :: [Prop] -> [VNode] -> VNode
option_ = mkElement "option"

button_ :: [Prop] -> [VNode] -> VNode
button_ = mkElement "button"

-- Media / links
img_ :: [Prop] -> VNode
img_ = mkVoid "img"

a_ :: [Prop] -> [VNode] -> VNode
a_ = mkElement "a"

-- Fragment & null
fragment_ :: [VNode] -> VNode
fragment_ = VFragment

nullElem :: VNode
nullElem = VNull

-- | Helper for rendering keyed lists (like mapping over arrays in JSX).
--
-- @
-- keyedList_ "ul" [] (map (\\item -> (itemId item, li_ [] [text_ (itemName item)])) items)
-- @
keyedList_ :: String -> [Prop] -> [(Key, VNode)] -> VNode
keyedList_ tag props items =
  let (attrs, events, _) = toAttrsAndEvents props
  in VKeyed tag attrs events items
