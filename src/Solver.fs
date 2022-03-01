module Solver
open GridGrid
open Grid
open Browser.Dom

let checkFor = [1..9]

let private getRow grid sect cell =
    let _,y = getGlobalCoord sect cell
    [for s in grid do
     for c in s.Cells do
        let _,y2 = getGlobalCoord s c
        match c.Value with
        | Mark v when y2 = y -> yield v
        | _ -> ()
        ]

let private getColumn grid sect cell =
    let x,_ = getGlobalCoord sect cell
    [for s in grid do
     for c in s.Cells do
        let x2,_ = getGlobalCoord s c
        match c.Value with
        | Mark v when x = x2 -> yield v
        | _ -> ()
        ]

let private getSquare sect =
    [for cell in sect.Cells do
        match cell.Value with
        | Mark v -> yield v
        | _ -> ()]

let updateCells filter action grid = 
    [for sect in grid do
        yield {sect with Cells = [for cell in sect.Cells do
                                    if filter grid sect cell then 
                                        yield action grid sect cell
                                    else yield cell]}]

let private pencilCell grid sect cell =
    let unavailable = getColumn grid sect cell
                    |> List.append (getRow grid sect cell)
                    |> List.append (getSquare sect)
    let result = checkFor |> List.filter (fun i -> unavailable |> List.contains i |> not)
    {cell with Value = PencilMark result}

let pencil grid =
    grid |> updateCells (fun g s c -> match c.Value with | Empty -> true | PencilMark _ -> true | _ -> false) pencilCell
 
let convertPencilToMarks grid =
    grid |> updateCells (fun g s c -> true) (fun g s c -> match c.Value with | PencilMark v when v.Length = 1 -> {c with Value = Mark(v.Head)} | _ -> c)
   

let private bestSection grid =
    let section = [for sect in grid -> 
                    (sect, sect.Cells 
                            |> List.map (fun r -> match r.Value with | PencilMark v -> Some v.Length | _ -> None)
                            |> List.choose id
                            |> List.sum)]

    let state = section.Head

    let folder accumulator current =
        let _,score = accumulator
        let _,cscore = current
        if cscore > score then
            current
        else accumulator

    let s,_ = section |> List.fold folder state
    s

let private bestCell sect =
    let cells = [for cell in sect.Cells -> cell, match cell.Value with | PencilMark v -> v.Length | _ -> 0]

    let folder acc c =
        let _,a = acc
        let _,ca = c
        if ca > a then c else acc
    
    let c,_ = cells |> List.fold folder (cells.Head)
    c

let updateCell sect cell grid =
    grid |> updateCells (fun g s c -> getGlobalCoord sect cell = getGlobalCoord s c) (fun g s c -> cell)

let guess grid = 
    let sect = bestSection grid
    let cell = bestCell sect
    grid 
    |> updateCell sect {cell with Value = match cell.Value with | PencilMark v -> Mark(v.Head) | _ -> cell.Value }