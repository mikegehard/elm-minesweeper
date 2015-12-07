module Main where

import StartApp
import Effects exposing (Effects)
import Minesweeper.Game
import Task exposing (Task)
import Effects
import Html exposing (Html)

app =
  StartApp.start { 
    init = init,
    view = Minesweeper.Game.view,
    update = Minesweeper.Game.update, inputs = []
  }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks

port startTime : Int

init : (Minesweeper.Game.Model, Effects Minesweeper.Game.Action)
init = (Minesweeper.Game.initial startTime, Effects.none)
