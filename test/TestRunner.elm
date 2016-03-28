module Main where

import Signal exposing (Signal)

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task

import GeoJsonTests

console : IO ()
console = consoleRunner GeoJsonTests.all

port runner : Signal (Task.Task x ())
port runner = run console
