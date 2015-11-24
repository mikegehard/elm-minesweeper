module Minesweeper where

type alias Board = List Row

type alias Row = List Tile

type alias Tile = {
  isMine: Bool
}

initialTile = {isMine = False}

createBoard: Int -> Board
createBoard size = List.repeat size (List.repeat size initialTile)
