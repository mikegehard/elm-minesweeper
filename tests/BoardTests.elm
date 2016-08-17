module BoardTests exposing (..)

import ElmTest exposing (..)
import Minesweeper.Board as Board
import Minesweeper.Tile as Tile


newBoard =
    let
        board =
            Board.create 1 0 1

        notFullyExposed =
            not << Board.determineIfFullyExposed
    in
        test "New board" (assert (notFullyExposed board.tiles))


allMarkedTest =
    let
        board =
            Board.create 1 0 1

        markedTiles =
            List.map Tile.mark board.tiles
    in
        test "All marked board" (assert (Board.determineIfFullyExposed markedTiles))


allExposedTest =
    let
        board =
            Board.create 1 0 1

        exposedTiles =
            List.map Tile.expose board.tiles
    in
        test "All exposed board" (assert (Board.determineIfFullyExposed exposedTiles))


determineIfFullyExposedTests =
    suite "determineIfFullyExposed"
        [ newBoard
        , allMarkedTest
        , allExposedTest
        ]


whenTileIdMatchesTest =
    let
        -- 2x2 board
        board =
            Board.create 2 0 1

        actual =
            board.tiles
                |> Board.expose 0
                |> Board.expose 2
                |> List.map .isExposed

        expected =
            [ True, False, True, False ]
    in
        test "When tile id matches" (assertEqual expected actual)


exposeTests =
    suite "expose"
        [ whenTileIdMatchesTest
        ]


all : Test
all =
    suite "BoardTests"
        [ determineIfFullyExposedTests
        , exposeTests
        ]
