module Completer where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import String exposing (contains)
import StartApp

type alias Word =
  {
    text:   String,
    weight: Int
  }

type alias Model =
  {
    words: List Word,
    suggestions: List Word,
    wordInput: String,
    autocompleteInput: String,
    completions: List Word
  }

type Action
  = NoOp
  | AddWord
  | UpdateWordInput String
  | Autocomplete String

buildWord : String -> Int -> Word
buildWord text weight =
  {
    text   = text,
    weight = weight
  }

buildCompletions : List Word -> String -> List Word
buildCompletions words input =
  let
    (matches, _) = (List.partition (\w -> String.contains input w.text) words)
  in
    matches

initialModel : Model
initialModel =
  {
    words       = [ buildWord "apple" 0, buildWord "pear" 0 ],
    suggestions = [ ],
    wordInput   = "",
    autocompleteInput = "",
    completions = [ ]
  }

wordItem : Word -> Html
wordItem word =
  li [ ] [ text word.text ]

listWords : List Word -> Html
listWords words =
  ul [ ] (List.map wordItem words)

wordForm : Address Action -> Model -> Html
wordForm address model =
  div [ ]
      [ input [ type' "text",
                placeholder "new word",
                value model.wordInput,
                on "input" targetValue (Signal.message address << UpdateWordInput)
              ] [ ],
        button [ onClick address AddWord ] [ text "add" ]
      ]

completerInput : Address Action -> Model -> Html
completerInput address model =
  div [ ]
      [ input [ type' "text",
                placeholder "type here",
                value model.autocompleteInput,
                on "input" targetValue (Signal.message address << Autocomplete)
              ] [ ]
      ]

update : Action -> Model -> Model
update action model =
  case action of
    NoOp    -> model
    AddWord ->
      let
        newWord = buildWord model.wordInput 0
      in
      { model |
        words <- newWord :: model.words,
        wordInput <- ""
      }
    UpdateWordInput word ->
      { model | wordInput <- word }
    Autocomplete input ->
      if String.isEmpty input
      then { model | autocompleteInput <- "", completions <- [ ] }
      else
        { model |
          autocompleteInput <- input,
          completions       <- buildCompletions model.words input
        }

view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
      [ wordForm address model,
        listWords model.words,
        completerInput address model,
        listWords model.completions
      ]


main : Signal Html
main =
  StartApp.start
    { model  = initialModel,
      view   = view,
      update = update
    }
