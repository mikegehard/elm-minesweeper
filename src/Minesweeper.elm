module Minesweeper (
  Board,
  Tile,
  createBoard,
  reveal,
  toGrid,
  expose,
  markTile)
  where

import Array exposing (Array, length)
import Random exposing (generate, initialSeed, int, list)
import Debug

type alias Board = {
  size: Int,
  tiles: Array Tile
}

type alias Tile = {
  id: Int,
  numberOfAdjacentMines: Int,
  isMine: Bool,
  isMarked: Bool,
  isExposed: Bool
}

newTile : Int -> Tile
newTile id = Tile id 0 False False False

createBoard: Int -> Int -> Board
createBoard s numberOfMines = {
  size = s,
  tiles = Array.initialize (s * s) newTile |> addMines numberOfMines |> addAdjacentMineValues s }

expose: Board -> Board
expose board =
  let
    exposeTile tile = {tile | isExposed = True}
  in
    {board | tiles = Array.map exposeTile board.tiles}

addMines: Int -> Array Tile -> Array Tile
addMines numberOfMines tiles =
  let
    bombPositionGenerator = list numberOfMines (int 0 (length tiles))
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

addAdjacentMineValues: Int -> Array Tile -> Array Tile
addAdjacentMineValues boardSize tiles =
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
              tile.id - 1, tile.id + 1,
              tile.id + boardSize - 1,
              tile.id + boardSize,
              tile.id + boardSize + 1
            ]
      in
        Array.filter (\tile -> List.member tile.id neighborIndexes) tiles
  in
    Array.map populateAdjacentMines tiles

toGrid: Board -> List(List Tile)
toGrid board =
  let
    partition: List Tile -> List(List Tile)
    partition list =
      if List.length list == board.size then
        [list]
      else
        [List.take board.size list] ++ (List.drop board.size list |> partition)
  in
    Array.toList board.tiles |> partition

reveal: Tile -> Board -> Board
reveal tile board =
  {board | tiles = Array.set tile.id {tile | isExposed = True} board.tiles}

markTile: Tile -> Board -> Board
markTile tile board =
  {board | tiles = Array.set tile.id {tile | isMarked = True} board.tiles}
