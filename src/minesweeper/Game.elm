module Minesweeper.Game where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Effects exposing (Effects)
import Minesweeper.Board

type alias Model = {
  outcome: Maybe Outcome,
  board: Maybe Minesweeper.Board.Model
}

type Action = NoOp | Select Difficulty | UpdateBoard Minesweeper.Board.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Select difficulty ->
      ({model | board = Just(boardFor difficulty)}, Effects.none)
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

type Difficulty = Beginner | Intermediate | Advanced

type Outcome = Lost

initial = { board = Nothing, outcome = Nothing }

translateDifficulty: String -> Difficulty
translateDifficulty optionValue =
  case optionValue of
    "Beginner" -> Beginner
    "Intermediate" -> Intermediate
    "Advanced" -> Advanced
    _ -> Beginner

boardFor: Difficulty -> Minesweeper.Board.Model
boardFor difficulty =
  case difficulty of
    Beginner ->
      Minesweeper.Board.create 9 10
    Intermediate ->
      Minesweeper.Board.create 16 40
    Advanced ->
      Minesweeper.Board.create 22 99