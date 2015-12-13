module Minesweeper.Board where

import Array exposing (Array)
import Minesweeper.Tile exposing (Tile)
import Random exposing (generate, initialSeed, int, list)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Effects exposing (Effects)
import Json.Decode
import Minesweeper.Tile as Tile

type alias Model = {
  size: Int,
  tiles: List Tile,
  hitMine: Bool,
  isFullyExposed: Bool
}

type Action = Click Tile | Mark Tile

update : Action -> Model -> (Model, Effects Action)
update action board =
  case action of
  Click tile ->
    if tile.isMine then
      let
        exposedBoard = exposeAll board
      in
        ({exposedBoard | hitMine = True}, Effects.none)
    else
      let
        updatedBoard = expose tile.id board
      in
        ({updatedBoard | isFullyExposed = determineIfFullyExposed updatedBoard}, Effects.none)
  Mark tile ->
      (mark tile.id board, Effects.none)

view : Address Action -> Model -> Html
view address board =
  let
    displayTile: Tile -> Html
    displayTile tile =
      td
      [
        class (Minesweeper.Tile.classFor tile),
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

create: Int -> Int -> Int -> Model
create s numberOfMines randomSeed =
  {
    hitMine = False,
    isFullyExposed = False,
    size = s,
    tiles =
      Array.initialize (s * s) Minesweeper.Tile.new
      |> addMines randomSeed numberOfMines
      |> addAdjacentMineValues
      |> Array.toList
  }

determineIfFullyExposed: Model -> Bool
determineIfFullyExposed board =
  List.all (\tile -> tile.isExposed || tile.isMarked) board.tiles

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
    partition board.tiles

exposeAll: Model -> Model
exposeAll board =
  {board | tiles = List.map Tile.expose board.tiles}

expose: Int -> Model -> Model
expose tileId board =
  {board | tiles = updateMatchingTile tileId Tile.expose board.tiles}

mark: Int -> Model -> Model
mark tileId board =
  {board | tiles = updateMatchingTile tileId Tile.mark board.tiles}

updateMatchingTile: Int -> (Tile -> Tile) -> List Tile -> List Tile
updateMatchingTile id update tiles =
  List.map (\t -> if t.id == id then update t else t) tiles

onRightClick: Signal.Address a -> a -> Attribute
onRightClick address message =
  onWithOptions "contextmenu" {defaultOptions | preventDefault = True} Json.Decode.value (\_ -> Signal.message address message)

addMines: Int -> Int -> Array Tile -> Array Tile
addMines randomSeed numberOfMines tiles =
  let
    bombPositionGenerator = Random.list numberOfMines (int 0 (Array.length tiles))
    (bombPositions, _) = generate bombPositionGenerator (initialSeed randomSeed)
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
