module Main where

import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html, Attribute)
import Html.Attributes exposing (..)
import Minesweeper

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

--
-- My type declarations
--

type alias Model = Minesweeper.Board

boardSize = 3

type Action = NoOp

--
-- My functions
--
init : (Model, Effects Action)
init = (Minesweeper.createBoard boardSize, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

view : Address Action -> Model -> Html
view address model =
  let
    displayTile tile = Html.td [class "tile"] []
    displayRow row = Html.tr [] (List.map displayTile row)
  in
    Html.table [] (List.map displayRow model)
