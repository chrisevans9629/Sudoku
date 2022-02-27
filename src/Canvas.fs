module Canvas

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser
// Get the canvas context for drawing
let canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
let context = canvas.getContext_2d()

// Format RGB color as "rgb(r,g,b)"
let ($) s n = s + n.ToString()

let clear() =
    context.clearRect(0.,0., canvas.width, canvas.height)
    canvas.width <- canvas.width

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
    ctx.closePath()

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

let getWidth() =
    [document.body.scrollWidth
     document.documentElement.scrollWidth
     document.body.offsetWidth
     document.documentElement.offsetWidth
     document.documentElement.clientWidth] |> List.max
  


let getHeight() =
    [
    document.body.scrollHeight
    document.documentElement.scrollHeight
    document.body.offsetHeight
    document.documentElement.offsetHeight
    document.documentElement.clientHeight] |> List.max

let getWindowDimensions () =
    // let dim = [getHeight();getWidth()] |> List.min
    // canvas.width <- dim
    // canvas.height <- dim
    // console.log dim
    // dim,dim
    

    canvas.width,canvas.height

let setWindowSize (w,h) =
    context.canvas.width <- w
    context.canvas.height <- h

/// Get the first <img /> element and set `src` (do
/// nothing if it is the right one to keep animation)
let image (src:string) =
    let image = document.getElementsByTagName("img").[0] :?> HTMLImageElement
    if image.src.IndexOf(src) = -1 then image.src <- src
    image
