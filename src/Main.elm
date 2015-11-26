module Main where

import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Minesweeper exposing (Board, Tile, createBoard)

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
  status: Maybe Status,
  board: Maybe Board
}

initialModel = { board = Nothing, status = Nothing }

boardSize = 3

type Action = NoOp | Click Tile Board | Select Difficulty

type Difficulty = Beginner | Intermediate | Advanced

type Status = Lost

translateDifficulty: String -> Difficulty
translateDifficulty optionValue =
  case optionValue of
    "Beginner" -> Beginner
    "Intermediate" -> Intermediate
    "Advanced" -> Advanced
    _ -> Beginner

endGame: Model -> Model
endGame model =
  {model | board = Nothing, status = Just Lost}

init : (Model, Effects Action)
init = (initialModel, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Select difficulty ->
      case difficulty of
        Beginner ->
          ({model | board = Just(createBoard 9 10)}, Effects.none)
        Intermediate ->
          ({model | board = Just(createBoard 16 40)}, Effects.none)
        Advanced ->
          ({model | board = Just(createBoard 22 99)}, Effects.none)
    Click tile board ->
      if tile.isMine then
        (endGame model, Effects.none)
      else
        ({model | board = Just(Minesweeper.clickTile tile board)}, Effects.none)

view : Address Action -> Model -> Html
view address model =
  case model.board of
    Just board ->
      let
        classFor tile = if tile.isClicked then
            if tile.isMine then
              "tile clicked mine"
            else
              "tile clicked"
          else
            "tile"
        displayTile tile = td [class (classFor tile), onClick address (Click tile board)] []
        displayRow row = List.map displayTile row |> tr []
        tableHtml = Minesweeper.toGrid board |> List.map displayRow
      in
        table [] tableHtml
    Nothing ->
      case model.status of
        Just status ->
          h1 [] [text "Sorry you lost!!"]
        Nothing ->
          div [class "controls"]
          [
            select [on "change" targetValue (Signal.message address << Select << translateDifficulty)]
            [
              option [] [text "Select a difficulty..."],
              option [] [text "Beginner"],
              option [] [text "Intermediate"],
              option [] [text "Advanced"]
            ]
          ]
