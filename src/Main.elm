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

type Action = NoOp | Select Difficulty | Click Tile | Mark Tile

init : (Model, Effects Action)
init = (initialModel, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Select difficulty ->
      ({model | board = Just(Game.boardFor difficulty)}, Effects.none)
    Click tile ->
      case model.board of
        Just board ->
          if tile.isMine then
            ({model | board = Just(Minesweeper.Board.expose board), outcome = Just Game.Lost}, Effects.none)
          else
            ({model | board = Just(Minesweeper.Board.reveal tile board)}, Effects.none)
        Nothing -> (model, Effects.none)
    Mark tile ->
      case model.board of
        Just board -> ({model | board = Just(Minesweeper.Board.mark tile board)}, Effects.none)
        Nothing -> (model, Effects.none)

view : Address Action -> Model -> Html
view address model =
  let
    classFor: Tile -> String
    classFor tile =
      if tile.isMarked then
        "tile marked"
      else if tile.isExposed then
        if tile.isMine then
          "tile exposed mine"
        else
          "tile exposed"
      else
        "tile"

    displayTile: Board -> Tile -> Html
    displayTile board tile =
      td
      [
        class (classFor tile),
        onClick address (Click tile),
        onRightClick address (Mark tile)
      ]
      [ tile |> Minesweeper.Tile.textFor |> text]

    displayRow: Board -> List Tile -> Html
    displayRow board row = row |> List.map (displayTile board) |> tr []

    displayBoard: Maybe Board -> Maybe Html
    displayBoard board = board |> Maybe.map (\board -> Minesweeper.Board.toGrid board |> List.map (displayRow board) |> table [])

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

onRightClick: Signal.Address a -> a -> Attribute
onRightClick address message = onWithOptions "contextmenu" {defaultOptions | preventDefault = True} Json.Decode.value (\_ -> Signal.message address message)
