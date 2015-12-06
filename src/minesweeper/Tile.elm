module Minesweeper.Tile where

type alias Tile = {
  id: Int,
  numberOfAdjacentMines: Int,
  isMine: Bool,
  isMarked: Bool,
  isExposed: Bool
}

new : Int -> Tile
new id = Tile id 0 False False False

textFor: Tile -> String
textFor tile =
  if tile.isExposed && not tile.isMine then
     toString tile.numberOfAdjacentMines
  else
    ""

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
