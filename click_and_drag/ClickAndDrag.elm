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


findById : Int -> List Object -> Object
findById id list =
  let
    found = List.head (List.filter (\o -> o.id == id) list)
  in
    case found of
      Just obj -> obj
      Nothing -> newObject "nothing" 0


update : Action -> Model -> Model
update action model =
  case action of

    NoOp ->
      model

    Dropped side ->
      let
        obj            = findById model.dragging (model.left ++ model.right)
        remainingLeft  = List.filter (\o -> o /= obj) model.left
        remainingRight = List.filter (\o -> o /= obj) model.right
      in

      if side == "right"

      then { model |
             right <- obj :: remainingRight
           , left  <- remainingLeft
           , dragging <- 0 }

      else { model |
             left <- obj :: remainingLeft
           , right <- remainingRight
           , dragging <- 0 }


    Dragging id ->
      { model | dragging <- id}


-- Model

type alias Model =
  { left     : List Object
  , right    : List Object
  , dragging : Int
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
