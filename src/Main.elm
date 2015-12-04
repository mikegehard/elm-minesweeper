module Main where

import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Minesweeper.Board exposing (Board)
import Minesweeper.Tile exposing (Tile)
import Game exposing (Outcome, Difficulty, translateDifficulty)
import Json.Decode

app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

type alias Model = {
  outcome: Maybe Outcome,
  board: Maybe Board
}

initialModel = Game.initial

type Action = NoOp | Select Difficulty | UpdateBoard Minesweeper.Board.Action

init : (Model, Effects Action)
init = (initialModel, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Select difficulty ->
      ({model | board = Just(Game.boardFor difficulty)}, Effects.none)
    UpdateBoard action ->
      case model.board of
        Just board ->
          let
            (board, effects) = Minesweeper.Board.update action board
          in
            ({model | board = Just board}, Effects.map UpdateBoard effects)
        Nothing ->
          (model, Effects.none)

view : Address Action -> Model -> Html
view address model =
  let
    controlsHtml = div [class "controls"]
      [
        -- This use of << is a bit magical
        -- TODO: Take a look at how it works.
        select [on "change" targetValue (Signal.message address << Select << translateDifficulty)]
        [
          option [] [text "Select a difficulty..."],
          option [] [text "Beginner"],
          option [] [text "Intermediate"],
          option [] [text "Advanced"]
        ]
      ]

    toHtml: Outcome -> Html
    toHtml outcome = div [class "outcome"] [outcome |> toString |> text]

    outcomeHtml = model.outcome |> Maybe.map toHtml |> Maybe.withDefault (text "")

    -- This forwardTo stuff is still a bit magical.
    -- TODO: look closer at it.
    boardHtml = model.board |> Maybe.map (Minesweeper.Board.view (Signal.forwardTo address UpdateBoard))

    htmlElements = Maybe.withDefault controlsHtml boardHtml :: [outcomeHtml]
  in
    div [] htmlElements
