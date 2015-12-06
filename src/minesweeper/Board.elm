module Minesweeper.Board
  (
    Model,
    Action,
    update,
    view,
    create
  )
  where

import Array exposing (Array)
import Minesweeper.Tile exposing (Tile)
import Random exposing (generate, initialSeed, int, list)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Effects exposing (Effects)
import Json.Decode

type alias Model = {
  size: Int,
  tiles: Array Tile
}

type Action = Click Tile | Mark Tile

update : Action -> Model -> (Model, Effects Action)
update action board =
  case action of
  Click tile ->
    if tile.isMine then
      (expose board, Effects.none)
    else
      (reveal tile board, Effects.none)
  Mark tile ->
      (mark tile board, Effects.none)

view : Address Action -> Model -> Html
view address board =
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

    displayTile: Tile -> Html
    displayTile tile =
      td
      [
        class (classFor tile),
        onClick address (Click tile),
        onRightClick address (Mark tile)
      ]
      [ tile
        |> Minesweeper.Tile.textFor
        |> text
      ]

    displayRow: List Tile -> Html
    displayRow row =
      row
      |> List.map displayTile
      |> tr []

  in
    toGrid board
    |> List.map displayRow
    |> table []

create: Int -> Int -> Model
create s numberOfMines =
  {
    size = s,
    tiles =
      Array.initialize (s * s) Minesweeper.Tile.new
      |> addMines numberOfMines
      |> addAdjacentMineValues
  }

-- Unexported Methods

toGrid: Model -> List(List Tile)
toGrid board =
  let
    partition: List Tile -> List(List Tile)
    partition list =
      if List.length list == board.size then
        [list]
      else
        [List.take board.size list] ++ (List.drop board.size list |> partition)
  in
    Array.toList board.tiles
    |> partition

expose: Model -> Model
expose board =
  let
    exposeTile tile = {tile | isExposed = True}
  in
    {board | tiles = Array.map exposeTile board.tiles}

reveal: Tile -> Model -> Model
reveal tile board =
  {board | tiles = Array.set tile.id {tile | isExposed = True} board.tiles}

mark: Tile -> Model -> Model
mark tile board =
  {board | tiles = Array.set tile.id {tile | isMarked = True} board.tiles}

onRightClick: Signal.Address a -> a -> Attribute
onRightClick address message =
  onWithOptions "contextmenu" {defaultOptions | preventDefault = True} Json.Decode.value (\_ -> Signal.message address message)

addMines: Int -> Array Tile -> Array Tile
addMines numberOfMines tiles =
  let
    bombPositionGenerator = Random.list numberOfMines (int 0 (Array.length tiles))
    (bombPositions, _) = generate bombPositionGenerator (initialSeed 101)
    insertMines: List(Int) -> Array Tile -> Array Tile
    insertMines listOfPositions tiles =
      case listOfPositions of
        [] -> tiles
        head::tail ->
          let
            currentTile = Array.get head tiles
          in
            case currentTile of
              Just tile -> insertMines tail (Array.set head {tile | isMine = True} tiles)
              Nothing -> tiles
  in
    insertMines bombPositions tiles

addAdjacentMineValues: Array Tile -> Array Tile
addAdjacentMineValues tiles =
  let
    populateAdjacentMines: Tile -> Tile
    populateAdjacentMines tile =
      { tile | numberOfAdjacentMines = calculateNeighboringMines tile }

    calculateNeighboringMines: Tile -> Int
    calculateNeighboringMines tile =
      neighbors tile |> Array.filter .isMine  |> Array.length

    neighbors: Tile -> Array Tile
    neighbors tile =
      let
        boardSize =
          Array.length tiles
          |> toFloat
          |> sqrt
          |> round

        isNWCorner tile = tile.id == 0
        isNECorner tile = tile.id == boardSize - 1
        isSWCorner tile = tile.id == (boardSize - 1) * boardSize
        isSECorner tile = tile.id == (boardSize * boardSize) - 1
        isTopRow tile = tile.id // boardSize == 0
        isBottomRow tile = tile.id // boardSize == boardSize - 1
        isLeftMostRow tile= tile.id `rem` boardSize == 0
        isRightMostRow tile = tile.id `rem` boardSize == boardSize - 1

        neighborIndexes =
          if isNWCorner tile then
            [
              tile.id + 1,
              tile.id + boardSize,
              tile.id + boardSize + 1
            ]
          else if isNECorner tile then
            [
              tile.id - 1,
              tile.id + boardSize,
              tile.id + boardSize + 1
            ]
          else if isSWCorner tile then
            [
              tile.id - boardSize,
              tile.id - boardSize + 1,
              tile.id + 1
            ]
          else if isSECorner tile then
            [
              tile.id - boardSize - 1,
              tile.id - boardSize,
              tile.id - 1
            ]
          else if isTopRow tile then
            [
              tile.id - 1,
              tile.id + 1,
              tile.id + boardSize - 1,
              tile.id + boardSize,
              tile.id + boardSize + 1
            ]
          else if isBottomRow tile then
            [
              tile.id - boardSize - 1,
              tile.id - boardSize,
              tile.id - boardSize + 1,
              tile.id - 1,
              tile.id + 1
            ]
          else if isLeftMostRow tile then
            [
              tile.id - boardSize,
              tile.id - boardSize + 1,
              tile.id + 1,
              tile.id + boardSize,
              tile.id + boardSize + 1
            ]
          else if isRightMostRow tile then
            [
              tile.id - boardSize - 1,
              tile.id - boardSize,
              tile.id - 1,
              tile.id + boardSize - 1,
              tile.id + boardSize
            ]
          else
            [
              tile.id - boardSize - 1,
              tile.id - boardSize,
              tile.id - boardSize + 1,
              tile.id - 1,
              tile.id + 1,
              tile.id + boardSize - 1,
              tile.id + boardSize,
              tile.id + boardSize + 1
            ]
      in
        Array.filter (\tile -> List.member tile.id neighborIndexes) tiles
  in
    Array.map populateAdjacentMines tiles
