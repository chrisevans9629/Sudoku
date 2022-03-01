module App

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser
                   

open Renderer
open Canvas


let getActSize () =
    let w,h = window.innerWidth,window.innerHeight

    let size = [w;h] |> List.min

    let padding = size / 10.

    let actSize = size - padding

    setWindowSize (actSize, actSize)
    actSize

let getOptions() =
    
    let actSize = getActSize()

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
        strokeRatio=0.005
        SelectedCell=Some (0,0)
        HighlightedCell=Some (1,2)
        stroke=stroke}
    options

let grid = SampleGrid.uniqueCandidate
                |> Solver.pencil


let options = getOptions()



//Canvas.mouseMove (fun x y -> )

//let solved = Solver.pencil grid

//let solved2 = Solver.convertPencilToMarks solved


let btnSolve = document.getElementById("btn-solve") :?> HTMLButtonElement
let btnGuess = document.getElementById("btn-guess") :?> HTMLButtonElement

let solve grid =
    grid 
        |> Solver.convertPencilToMarks
        |> Solver.pencil

open Solver
open GridGrid
open Grid
let rec update grid options =
    render grid options

    btnGuess.onclick <- (fun e -> 
        let result = grid |> Solver.convertPencilToMarks |> Solver.pencil |> Solver.guess |> Solver.pencil
        update result options)

    btnSolve.onclick <- (fun e -> 
        let result = solve grid
        update result options)
    
    window.onresize <- (fun e -> 
        let actSize = getActSize()
        update grid {options with CellSize=actSize/9.; Width=actSize; Height=actSize})

    Canvas.mouseMove (fun (x,y) -> 
        update grid {options with HighlightedCell=Some (int(x/options.CellSize),int(y/options.CellSize)) })

    Canvas.mouseDown (fun (x,y) -> 
        update grid {options with SelectedCell=Some (int(x/options.CellSize),(int(y/options.CellSize)))})

    Canvas.keyDown (fun key -> 
        console.log(key)

        match options.SelectedCell, key with
        | Some xy, k when checkFor |> List.map (fun r -> r |> string) |> List.contains k -> 
            let result = grid |> updateCells (fun _ s c -> xy = getGlobalCoord s c) (fun _ _ c -> {c with Value = Mark(k |> int)}) |> Solver.pencil
            update result options
        | Some xy, k when k = "Delete" ->
            let result = grid |> updateCells (fun _ s c -> xy = getGlobalCoord s c) (fun g s c -> {c with Value=Empty}) |> Solver.pencil
            update result options
        | _, _ -> ()
        )

update grid options