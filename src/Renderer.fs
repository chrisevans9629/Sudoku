module Renderer
open GridGrid
open Grid

let rgb r g b:string = sprintf "rgb(%i,%i,%i)" r g b

let white = rgb 255 255 255
let black = rgb 0 0 0
let red = rgb 100 0 0
type Font =
    | DebugFont
    | MarkFont
    | PencilFont

type RenderOptions = {
    CellSize:float
    Width:float
    Height:float
    Debug:bool
    filled:(string -> (float * float * float * float) -> unit)
    line:(float -> string -> (float * float) -> (float * float) -> unit)
    text:int -> (float * float) -> string -> string -> unit
    clear:unit -> unit
    fontRatio:float
    strokeRatio:float
}

let getFontSize font options =
    //let ratio = 0.05
    match font with
    | DebugFont -> int(options.Width * options.fontRatio / 2.)
    | MarkFont -> int(options.Width * options.fontRatio)
    | PencilFont -> int(options.Width * options.fontRatio / 3.)

let drawBackground options =
    (0., 0., options.Width, options.Height) |> options.filled white
    
    let drawLines x w clr =
        options.line (w * options.Width) clr (x,0.) (x,options.Height) 
        options.line (w * options.Width) clr (0.,x) (options.Width, x)

    for c in 0..9 do
        let x = float(c) * options.CellSize
        drawLines x options.strokeRatio black
    
    
    for t in 0..3 do
        let x = float(t) * options.CellSize * 3.
        drawLines x (options.strokeRatio * 2.) red


let drawCell (cell:CellModel) ((x,y):float * float) (options:RenderOptions) sect :unit =
    let cX = float(cell.Column) * options.CellSize + x
    let cY = float(cell.Row) * options.CellSize + y
    let middle = (cX+options.CellSize/2.,cY+options.CellSize/2.)

    let top = (cX+options.CellSize/2.,cY + 20.)
    let bottom = (cX + options.CellSize/2., cY+options.CellSize - 20.)
    
    match options.Debug, cell.Value with
    | _, Empty -> ()
    | true,_->
        let gX,gY = getGlobalCoord sect cell 

        let debugFont = getFontSize DebugFont options

        options.text debugFont bottom black (sprintf "global: (%i,%i)" gX gY)
        
        //text 10 top (rgb 0 0 0) (sprintf "local: (%i,%i)" cell.Column cell.Row)

        let lSX,lSY,lx,ly = getLocalCoord (gX, gY)

        options.text debugFont top black (sprintf "local (calc): (%i,%i) (%i %i)" lSX lSY lx ly)
    | _,_ -> ()

    match cell.Value with
    | Mark v -> v.ToString() |> options.text (getFontSize MarkFont options) middle black
    | PencilMark values -> 
        values |> List.sortDescending 
        |> List.map (fun v -> v.ToString()) 
        |> List.reduce (fun f s -> s + f) 
        |> options.text (getFontSize PencilFont options) middle black
    | Empty -> ()

let drawSect (sect:GridCell) options =
    let sectSize = options.CellSize * 3.
    let x = float(sect.Column) * sectSize
    let y = float(sect.Row) * sectSize

    if options.Debug then
        options.text (getFontSize DebugFont options) (x+30.,y+10.) (rgb 255 0 0) (sprintf "sect: (%i,%i)" sect.Column sect.Row)
    //(x,y,x+sectSize,y+sectSize) |> filled (rgb x y 0)

    for cell in sect.Cells do
        drawCell cell (float(sect.Column) * sectSize, float(sect.Row) * sectSize) options sect

let render grid options =
    options.clear()
    drawBackground options
    for sect in grid do
        drawSect sect options
