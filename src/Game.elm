module Game where

import Minesweeper.Board exposing (Board)

type Difficulty = Beginner | Intermediate | Advanced

type Outcome = Lost

initial = { board = Nothing, outcome = Nothing }

translateDifficulty: String -> Difficulty
translateDifficulty optionValue =
  case optionValue of
    "Beginner" -> Beginner
    "Intermediate" -> Intermediate
    "Advanced" -> Advanced
    _ -> Beginner

boardFor: Difficulty -> Minesweeper.Board.Board
boardFor difficulty =
  case difficulty of
    Beginner ->
      Minesweeper.Board.create 9 10
    Intermediate ->
      Minesweeper.Board.create 16 40
    Advanced ->
      Minesweeper.Board.create 22 99
