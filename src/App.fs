module App

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser


// // Mutable variable to count the number of times we clicked the button
// let mutable count = 0

// // Get a reference to our button and cast the Element to an HTMLButtonElement
// let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

// // Register our listener
// myButton.onclick <- fun _ ->
//     count <- count + 1
//     myButton.innerText <- sprintf "You clicked: %i time(s)" count

module Grid =
    type CellValue = PencilMark of int list | Mark of int
    type CellModel = {Column:int; Row:int;Value:CellValue }

    let mark (c,r) v = {Column=c;Row=r;Value=Mark(v)}
    let pencil (c,r) v = {Column=c;Row=r;Value=PencilMark(v)}
module GridGrid = 
    type GridCell = {Column:int; Row:int;Cells:Grid.CellModel list}
    let private gridSect (c,r) cells =
        {Column=c;Row=r;Cells=cells}
    
    let getGlobalCoord (sect:GridCell) (cell:Grid.CellModel) =
        (sect.Column * 3 + cell.Column, sect.Row * 3 + cell.Row)

    let getLocalCoord (x,y) =
        let sectX,sectY = (x / 3, y / 3)
        let lx,ly = (x - sectX * 3, y - sectY * 3)
        (sectX,sectY, lx , ly)

    let private baseGrid =
        [for c in 0..2 do 
         for r in 0..2 -> gridSect (c,r) []]

    let private addMark (x,y,v) grid =
        let sectX,sectY,lx,ly = getLocalCoord (x,y)
        [
            for sect in grid do
            if sect.Column = sectX && sect.Row = sectY then
                yield {sect with Cells = sect.Cells |> List.append [(Grid.mark (lx,ly) v)]}
            else yield sect
        ]

    let rec private createMarks grid list =
        match list with
        | head :: tail ->
            let result = grid |> addMark head
            createMarks result tail
        | [] -> grid

    let create list =
        createMarks baseGrid list
    

module SampleGrid = 
    open Grid
    open GridGrid

    let samGrid = create [
        (0,2,5)
        (2,0,1)
        
    ]
    // let samGrid = [
    //     gridSect (0,0) [
    //         mark (0,2) 5
    //         mark (2,0) 1
    //         ]
    //     gridSect (1,0) [
    //         mark (0,0) 2
    //         mark (0,1) 6
    //         mark (2,0) 3
    //         mark (2,1) 7
    //     ]
    //     gridSect (2,0) [
    //         mark (0,0) 4
    //         mark (2,2) 3
    //         pencil (1,1) [1;2;3]
    //     ]
    //     gridSect (0,1) []
    //     gridSect (1,1) []
    //     gridSect (2,1) []
    //     gridSect (0,2) []
    //     gridSect (1,2) []
    //     gridSect (2,2) []
        
    // ]

                        
module Canvas =

    // Get the canvas context for drawing
    let canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    let context = canvas.getContext_2d()

    // Format RGB color as "rgb(r,g,b)"
    let ($) s n = s + n.ToString()
    let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

    /// Fill rectangle with given color
    let filled (color: string) rect =
        let ctx = context
        ctx.fillStyle <- !^ color
        ctx.fillRect rect

    let stroke lineWidth (color: string) rect =
        let ctx = context
        ctx.strokeStyle <- !^ color
        ctx.lineWidth <- lineWidth
        ctx.strokeRect rect

    let line lineWidth (color: string) from lTo =
        let ctx = context
        ctx.beginPath()
        ctx.strokeStyle <- !^ color
        ctx.lineWidth <- lineWidth
        ctx.moveTo from
        ctx.lineTo lTo
        ctx.stroke()

    let text size (x,y) (color: string) txt =
        let ctx = context
        ctx.font <- sprintf "%ipx Arial" size
        ctx.textAlign <- "center"
        ctx.fillStyle <- !^ color
        ctx.textBaseline <- "middle"
        ctx.fillText (txt, x, y)


    /// Move element to a specified X Y position
    let position (x,y) (img : HTMLImageElement) =
        img?style?left <- x.ToString() + "px"
        img?style?top <- (canvas.offsetTop + y).ToString() + "px"

    let getWindowDimensions () =
        canvas.width, canvas.height

    /// Get the first <img /> element and set `src` (do
    /// nothing if it is the right one to keep animation)
    let image (src:string) =
        let image = document.getElementsByTagName("img").[0] :?> HTMLImageElement
        if image.src.IndexOf(src) = -1 then image.src <- src
        image

open Canvas
open GridGrid
open Grid
module Renderer =
    type RenderOptions = {
        CellSize:float
        Width:float
        Height:float
        Padding:float
        Debug:bool
    }

    let drawBackground options =
        (0., 0., options.Width, options.Height) |> filled (rgb 250 255 255)
        
        let drawLines x w clr =
            line w clr (x,0.) (x,options.Height) 
            line w clr (0.,x) (options.Width, x)

        for c in 0..9 do
            let x = float(c) * options.CellSize
            drawLines x 0. (rgb 0 0 0)
        
        
        for t in 0..3 do
            let x = float(t) * options.CellSize * 3.
            drawLines x 10. (rgb 100 0 0)


    let drawCell (cell:CellModel) ((x,y):float * float) (options:RenderOptions) sect :unit =
        let cX = float(cell.Column) * options.CellSize + x
        let cY = float(cell.Row) * options.CellSize + y
        let middle = (cX+options.CellSize/2.,cY+options.CellSize/2.)

        let top = (cX+options.CellSize/2.,cY + 20.)
        let bottom = (cX + options.CellSize/2., cY+options.CellSize - 20.)
        if options.Debug then
            let gX,gY = getGlobalCoord sect cell 
            text 10 bottom (rgb 0 0 0) (sprintf "global: (%i,%i)" gX gY)
            
            //text 10 top (rgb 0 0 0) (sprintf "local: (%i,%i)" cell.Column cell.Row)

            let lSX,lSY,lx,ly = getLocalCoord (gX, gY)

            text 10 top (rgb 0 0 0) (sprintf "local (calc): (%i,%i) (%i %i)" lSX lSY lx ly)

        match cell.Value with
        | Mark v -> v.ToString() |> text  45 middle (rgb 0 0 0)
        | PencilMark values -> 
            values |> List.sortDescending 
            |> List.map (fun v -> v.ToString()) 
            |> List.reduce (fun f s -> s + f) 
            |> text 20 middle (rgb 0 0 0)

    let drawSect (sect:GridCell) options =
        let sectSize = options.CellSize * 3.
        let x = float(sect.Column) * sectSize
        let y = float(sect.Row) * sectSize

        if options.Debug then
            text 10 (x+30.,y+10.) (rgb 255 0 0) (sprintf "sect: (%i,%i)" sect.Column sect.Row)
        //(x,y,x+sectSize,y+sectSize) |> filled (rgb x y 0)

        for cell in sect.Cells do
            drawCell cell (float(sect.Column) * sectSize, float(sect.Row) * sectSize) options sect

    let render grid options =
        drawBackground options
        for sect in grid do
            drawSect sect options

open Renderer



let w,h = getWindowDimensions()

let options = {CellSize=w/9.; Width=w; Height=h;Padding=10.;Debug=true}

let rec update () =
    render SampleGrid.samGrid options
    //window.setTimeout(update, 1000/60) |> ignore

update()