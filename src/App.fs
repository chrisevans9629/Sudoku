module App

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser
                   

open Renderer
open Canvas


let getOptions() =
    let w,h = window.innerWidth,window.innerHeight

    let size = [w;h] |> List.min

    let padding = size / 10.

    let actSize = size - padding

    setWindowSize (actSize, actSize)

    let options = {
        CellSize=actSize / 9.
        Width=actSize
        Height=actSize
        Debug=false
        filled=filled
        text=text
        line=line
        clear=clear
        fontRatio=0.05
        strokeRatio=0.005}
    options

let mutable grid = SampleGrid.samGrid
                |> Solver.pencil

//let solved = Solver.pencil grid

//let solved2 = Solver.convertPencilToMarks solved

let rec update () =
    let options = getOptions()
    render grid options
    //window.setTimeout(update, 1000/60) |> ignore

let btn = document.getElementById("btn-solve") :?> HTMLButtonElement

let solve() =
    grid <- grid 
        |> Solver.convertPencilToMarks
        |> Solver.pencil
    update()

btn.addEventListener("click", (fun e -> solve()))
window.addEventListener("resize", (fun e -> update()))
update()