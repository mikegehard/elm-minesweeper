module Game where

import Minesweeper

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

boardFor: Difficulty -> Minesweeper.Board
boardFor difficulty =
  case difficulty of
    Beginner ->
      Minesweeper.createBoard 9 10
    Intermediate ->
      Minesweeper.createBoard 16 40
    Advanced ->
      Minesweeper.createBoard 22 99
