module App

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser
                   

open Renderer
open Canvas

let w,h = getWindowDimensions()

let options = {
    CellSize=w/9.
    Width=w
    Height=h
    Padding=10.
    Debug=false
    filled=filled
    text=text
    line=line}

let grid = SampleGrid.samGrid

let solved = Solver.pencil grid

let rec update () =
    render solved options
    //window.setTimeout(update, 1000/60) |> ignore

update()