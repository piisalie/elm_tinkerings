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
  | Dragging String


update : Action -> Model -> Model
update action model =
  case action of

    NoOp ->
      model

    Dropped side ->
      let
        remainingLeft = List.filter (\o -> o /= model.dragging) model.left
        remainingRight = List.filter (\o -> o /= model.dragging) model.right
      in

      if side == "right"

      then { model |
             right <- model.dragging :: remainingRight
           , left  <- remainingLeft
           , dragging <- "" }

      else { model |
             left <- model.dragging :: remainingLeft
           , right <- remainingRight
           , dragging <- "" }


    Dragging txt ->
      { model | dragging <- txt}


-- Model

type alias Model =
  { left  : List String
  , right : List String
  , dragging: String
  }


initialModel : Model
initialModel =
  { left  = [ "one", "two" ]
  , right = [ "three", "four" ]
  , dragging = ""
  }


type alias Options =
    { stopPropagation : Bool
    , preventDefault : Bool
    }


defaultOptions =
    { stopPropagation = False
    , preventDefault = True
    }


-- View

buildObject : Signal.Address Action -> String -> Html
buildObject address content =
  span [ class "object"
       , id content
       , draggable "true"
       , on "dragstart" Json.value (\_ -> Signal.message address (Dragging content))
       ]
       [ text content ]


items : Signal.Address Action -> String -> List String -> Html
items address side objs =
  let
    objects = List.map (buildObject address) objs
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
