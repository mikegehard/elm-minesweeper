module Main where

import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html, Attribute)
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

boardSize = 3

type Action = NoOp | Click Tile

init : (Board, Effects Action)
init = (createBoard boardSize, Effects.none)

update : Action -> Board -> (Board, Effects Action)
update action board =
  case action of
    NoOp -> (board, Effects.none)
    Click tile -> (Minesweeper.clickTile tile board, Effects.none)

view : Address Action -> Board -> Html
view address board =
  let
    classFor tile = if tile.isClicked then "tile clicked" else "tile"
    displayTile tile = Html.td [class (classFor tile), onClick address (Click tile)] []
    displayRow row = List.map displayTile row |> Html.tr []
    tableHtml = Minesweeper.toGrid board |> List.map displayRow
  in
    Html.table [] tableHtml
