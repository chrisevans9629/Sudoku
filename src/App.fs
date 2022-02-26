module App

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser


// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

// Register our listener
myButton.onclick <- fun _ ->
    count <- count + 1
    myButton.innerText <- sprintf "You clicked: %i time(s)" count

module Grid =
    type CellValue = PencilMark of int list | Mark of int
    type CellModel = {Column:int; Row:int;Value:CellValue }

    let mark (c,r) v = {Column=c;Row=r;Value=Mark(v)}

module GridGrid = 
    type GridCell = {Column:int; Row:int;Cells:Grid.CellModel list}
    let gridSect (c,r) cells =
        {Column=c;Row=r;Cells=cells}
module SampleGrid = 
    open Grid
    open GridGrid
    let samGrid = [
        gridSect (0,0) [
            mark (0,2) 5
            mark (2,0) 1
            ]
        gridSect (1,0) [
            mark (0,0) 2
            mark (0,1) 6
            mark (2,0) 3
            mark (2,1) 7
        ]
        gridSect (2,0) [
            mark (0,0) 4
            mark (2,2) 3
        ]
        gridSect (0,1) []
        gridSect (1,1) []
        gridSect (2,1) []
        gridSect (0,2) []
        gridSect (1,2) []
        gridSect (2,2) []
        
    ]


                        
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

    let stroke (color: string) rect =
        let ctx = context
        ctx.strokeStyle <- !^ color
        ctx.strokeRect rect

    let text txt size (x,y) (color: string) =
        let ctx = context
        ctx.font <- sprintf "%ipx Times New Roman" size
        ctx.fillStyle <- !^ color
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
    }

    let drawCell (cell:CellModel) ((x,y):float * float) (options:RenderOptions) :unit =
        ()

    let drawSect (sect:GridCell) options =
        let sectSize = options.CellSize * 3.
        let x = float(sect.Column) * sectSize
        let y = float(sect.Row) * sectSize

        (x+options.Padding,y+options.Padding,x+sectSize-options.Padding,y+sectSize-options.Padding) |> filled (rgb 0 0 0)

        for cell in sect.Cells do
            drawCell cell (float(sect.Column) * sectSize, float(sect.Row) * sectSize) options

    let render grid options =
        (0., 0., options.Width, options.Height) |> filled (rgb 255 255 255)
        for sect in grid do
            drawSect sect options



open Renderer



let w,h = getWindowDimensions()

let options = {CellSize=w/9.; Width=w; Height=h;Padding=10.}

let rec update () =
    render SampleGrid.samGrid options
    window.setTimeout(update, 1000/60) |> ignore

update()