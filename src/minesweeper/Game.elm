module Minesweeper.Game where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Effects exposing (Effects)
import Minesweeper.Board

type alias Model = {
  startTime: Int,
  outcome: Maybe Outcome,
  board: Maybe Minesweeper.Board.Model
}

type Action = NoOp | Select Difficulty | UpdateBoard Minesweeper.Board.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Select difficulty ->
      ({model | board = Just(boardFor difficulty model.startTime)}, Effects.none)
    UpdateBoard action ->
      case model.board of
        Just board ->
          let
            (board, effects) = Minesweeper.Board.update action board
            updatedModel =
              if board.hitMine then
                {model | board = Just board, outcome = Just Lost}
              else
                if board.isFullyExposed then
                  {model | board = Just board, outcome = Just Won}
                else
                  {model | board = Just board}
          in
            (updatedModel, Effects.map UpdateBoard effects)
        Nothing ->
          (model, Effects.none)

view : Address Action -> Model -> Html
view address model =
  let
    controlsHtml = div [class "controls"]
      [
        -- << and >> are function composition operators
        -- The next two lines are identical.
        select [on "change" targetValue (translateDifficulty >> Select >> Signal.message address)]
        -- select [on "change" targetValue (Signal.message address << Select << translateDifficulty)]
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

    boardHtml = model.board |> Maybe.map (Minesweeper.Board.view (Signal.forwardTo address UpdateBoard))

    htmlElements = [Maybe.withDefault controlsHtml boardHtml, outcomeHtml]
  in
    div [] htmlElements

type Difficulty = Beginner | Intermediate | Advanced

type Outcome = Lost | Won

initial: Int -> Model
initial startTime =
  {
    startTime = startTime,
    board = Nothing,
    outcome = Nothing
  }

translateDifficulty: String -> Difficulty
translateDifficulty optionValue =
  case optionValue of
    "Beginner" -> Beginner
    "Intermediate" -> Intermediate
    "Advanced" -> Advanced
    _ -> Beginner

boardFor: Difficulty -> Int -> Minesweeper.Board.Model
boardFor difficulty randomSeed =
  case difficulty of
    Beginner ->
      Minesweeper.Board.create 9 10 randomSeed
    Intermediate ->
      Minesweeper.Board.create 16 40 randomSeed
    Advanced ->
      Minesweeper.Board.create 22 99 randomSeed
