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

let mutable grid = SampleGrid.samGrid
                |> Solver.pencil

//let solved = Solver.pencil grid

//let solved2 = Solver.convertPencilToMarks solved

let rec update () =
    render grid options
    //window.setTimeout(update, 1000/60) |> ignore

let btn = document.getElementById("btn-solve") :?> HTMLButtonElement

let solve() =
    grid <- grid 
        |> Solver.convertPencilToMarks
        |> Solver.pencil
    update()

btn.addEventListener("click", (fun e -> solve()))

update()