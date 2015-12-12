module BoardTests where

import ElmTest exposing (..)
import Minesweeper.Board as Board
import Minesweeper.Tile as Tile
import Array

newBoard =
  let
    board = Board.create 1 0 1
    notFullyExposed = not << Board.determineIfFullyExposed
  in
    test "New board" (assert (notFullyExposed board))

allMarkedTest =
  let
    board = Board.create 1 0 1
    markedBoard = {board | tiles = Array.map Tile.mark board.tiles}
  in
    test "All marked board" (assert (Board.determineIfFullyExposed markedBoard))

allExposedTest =
  let
    board = Board.create 1 0 1
    exposedBoard = {board | tiles = Array.map Tile.expose board.tiles}
  in
    test "All exposed board" (assert (Board.determineIfFullyExposed exposedBoard))


all : Test
all =
    suite "A Test Suite"
        [
          newBoard,
          allMarkedTest,
          allExposedTest
        ]
