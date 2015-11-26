module Minesweeper where

import Array exposing (Array)

type alias Board = Array Tile

type alias Tile = {
  id: Int,
  isMine: Bool,
  isClicked: Bool
}

newTile : Int -> Tile
newTile id = {
  id = id,
  isMine = False,
  isClicked = False }

createBoard: Int -> Board
createBoard size = Array.initialize (size * size) newTile

toGrid: Board -> List(List Tile)
toGrid board =
  let
    size = Array.length board |> toFloat |> sqrt |> truncate
    partition: List Tile -> List(List Tile)
    partition list =
      if List.length list == size then
        [list]
      else
        [List.take size list] ++ (List.drop size list |> partition)
  in
    Array.toList board |> partition

clickTile: Tile -> Board -> Board
clickTile tile board = Array.set tile.id {tile | isClicked = True} board
