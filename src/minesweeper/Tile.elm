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

expose: Tile -> Tile
expose tile =
  {tile | isExposed = True}

mark: Tile -> Tile
mark tile =
  {tile | isMarked = True}

textFor: Tile -> String
textFor tile =
  if tile.isExposed && not tile.isMine then
     toString tile.numberOfAdjacentMines
  else
    ""

classFor: Tile -> String
classFor tile =
  let
    addMarked tile class =
      if tile.isMarked then
        class ++ " marked"
      else
        class

    addMine tile class =
      if tile.isMine then
        class ++ " mine"
      else
        class

    addExposed tile class =
      if tile.isExposed then
        class ++ " exposed"
      else
        class
  in
    "tile" |> addMarked tile |> addMine tile |> addExposed tile
