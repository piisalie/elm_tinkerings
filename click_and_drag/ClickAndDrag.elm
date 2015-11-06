module ClickAndDrag where

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Json.Decode as Json
import Result exposing(..)

import Debug exposing(..)


-- Update

type Action
  = NoOp
  | Dropped String
  | Dragging Int
  | OverObj Int


findById : Int -> List Object -> Object
findById id list =
  let
    found = List.head (List.filter (\o -> o.id == id) list)

  in
    case found of
      Just obj -> obj

      Nothing -> newObject "nothing" 0


insertObj : Object -> Object -> List Object -> List Object
insertObj object overObj list  =
  let
    filtered    = List.filter (\o -> o /= object) list
    beginning   = takeWhile (\e -> e /= overObj) filtered
    end         = dropWhile (\e -> e /= overObj) filtered
    countToOver = List.length ( takeWhile (\e -> e /= overObj) list )
    countToObj  = List.length ( takeWhile (\e -> e /= object) list )

    insertion = if | object == overObj        -> [ object ]
                   | countToObj < countToOver -> [ overObj, object ]
                   | countToObj > countToOver -> [ object, overObj ]
                   | otherwise                -> [ object ]

  in
    List.concat [ beginning, insertion, end ]


dropWhile : (a -> Bool) -> List a -> List a
dropWhile condition list =
  case list of

  [ ] -> [ ]

  first :: rest ->

    if (condition first) then
      dropWhile condition rest

    else
      rest


takeWhile : (a -> Bool) -> List a -> List a
takeWhile condition list =
  case list of

    [ ] -> [ ]

    first :: rest ->

      if (condition first) then
        first :: takeWhile condition rest

      else
        [ ]


update : Action -> Model -> Model
update action model =
  case action of

    NoOp ->
      model

    Dropped side ->
      let
        obj            = findById model.dragging (model.left ++ model.right)
        overObj        = findById model.overObj  (model.left ++ model.right)
        remainingLeft  = List.filter (\o -> o /= obj) model.left
        remainingRight = List.filter (\o -> o /= obj) model.right
      in

      if side == "right"

      then { model |
             right <- insertObj obj overObj model.right
           , left  <- remainingLeft
           , dragging <- 0
           , overObj  <- 0
           }

      else { model |
             left  <- insertObj obj overObj model.left
           , right <- remainingRight
           , dragging <- 0
           , overObj  <- 0
           }

    Dragging id ->
      { model | dragging <- id }

    OverObj id ->
      { model | overObj <- id }


-- Model

type alias Model =
  { left     : List Object
  , right    : List Object
  , dragging : Int
  , overObj  : Int
  }


type alias Options =
  { stopPropagation : Bool
  , preventDefault  : Bool
  }


type alias Object =
  { content : String
  , id      : Int
  }


newObject : String -> Int -> Object
newObject content id =
  { content = content
  , id      = id
  }


initialModel : Model
initialModel =
  { left     = [ newObject "one" 1
               , newObject "two" 2
               ]
  , right    = [ newObject "three" 3
               , newObject "four"  4
               ]
  , dragging = 0
  , overObj  = 0
  }


defaultOptions =
    { stopPropagation = False
    , preventDefault  = True
    }


-- View

buildObjectDiv : Signal.Address Action -> Object -> Html
buildObjectDiv address obj =
  div [ class "object"
       , id (toString obj.id)
       , draggable "true"
       , on "dragstart" Json.value (\_ -> Signal.message address (Dragging (obj.id)))
       , onWithOptions "dragover" defaultOptions Json.value (\_ -> Signal.message address (OverObj (obj.id)))
       ]
       [ text obj.content ]


items : Signal.Address Action -> String -> List Object -> Html
items address side objs =
  let
    objects = List.map (buildObjectDiv address) objs
  in
  div [ id side
      , onWithOptions "dragover" defaultOptions Json.value (\_ -> Signal.message address NoOp)
      , onWithOptions "drop" defaultOptions Json.value (\str ->
                                                          Signal.message address (Dropped side))
      ]
      objects


view : Signal.Address Action -> Model -> Html
view address model =
  div [ id "container" ]
      [ (items address "left" model.left)
      , (items address "right" model.right)
      ]


-- Signals

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp


actions : Signal Action
actions =
  inbox.signal


model : Signal Model
model =
  Signal.foldp update initialModel actions


-- Main

main : Signal Html
main =
  Signal.map (view inbox.address) model
