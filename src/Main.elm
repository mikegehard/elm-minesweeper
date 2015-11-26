module Main where

import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Minesweeper exposing (Board, Tile, createBoard, reveal, expose, toGrid)
import Game exposing (Outcome, Difficulty, translateDifficulty)

--
-- StartApp boilerplate
--
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

type Action = NoOp | Click Tile Board | Select Difficulty

init : (Model, Effects Action)
init = (initialModel, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Select difficulty ->
      ({model | board = Just(Game.boardFor difficulty)}, Effects.none)
    Click tile board ->
      if tile.isMine then
        ({model | board = Just(expose board), outcome = Just Game.Lost}, Effects.none)
      else
        ({model | board = Just(reveal tile board)}, Effects.none)

view : Address Action -> Model -> Html
view address model =
  let
    classFor: Tile -> String
    classFor tile =
      if tile.isExposed then
        if tile.isMine then
          "tile exposed mine"
        else
          "tile exposed"
      else
        "tile"

    displayTile: Board -> Tile -> Html
    displayTile board tile = td [class (classFor tile), onClick address (Click tile board)] []

    displayRow: Board -> List Tile -> Html
    displayRow board row = row |> List.map (displayTile board) |> tr []

    displayBoard: Maybe Board -> Maybe Html
    displayBoard board = board |> Maybe.map (\board -> toGrid board |> List.map (displayRow board) |> table [])

    controlsHtml = div [class "controls"]
      [
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

    boardHtml = displayBoard model.board

    htmlElements = [ boardHtml |> Maybe.withDefault controlsHtml ] ++ [outcomeHtml]
  in
    div [] htmlElements
