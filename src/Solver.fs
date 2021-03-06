module Solver
open GridGrid
open Grid
open Browser.Dom

let checkFor = [1..9]

let private getRow grid sect cell map =
    let _,y = getGlobalCoord sect cell
    [for s in grid do
     for c in s.Cells do
        let _,y2 = getGlobalCoord s c
        if y2 = y then yield map c]

let private getColumn grid sect cell map =
    let x,_ = getGlobalCoord sect cell
    [for s in grid do
     for c in s.Cells do
        let x2,_ = getGlobalCoord s c
        if x = x2 then yield map c]

let private getSquare sect map =
    [for cell in sect.Cells -> map cell]

let updateCells filter action grid = 
    [for sect in grid do
        yield {sect with Cells = [for cell in sect.Cells do
                                    if filter grid sect cell then 
                                        yield action grid sect cell
                                    else yield cell]}]

let private pencilCell grid sect cell =
    let map cell =
        match cell.Value with
        | Mark v -> v
        | PermaMark v -> v
        | _ -> -1
    
    let unavailable = getColumn grid sect cell map
                    |> List.append (getRow grid sect cell map)
                    |> List.append (getSquare sect map)
                    |> List.filter (fun r -> r > 0)
    let result = checkFor |> List.filter (fun i -> unavailable |> List.contains i |> not)
    {cell with Value = PencilMark result}

let eliminate grid =
    let getPencilMarks cell =
        match cell.Value with
        | PencilMark v -> v
        | _ -> []

    let updateCell get g s c  =
        let row = get g s c getPencilMarks
        
        let marks = getPencilMarks c |> List.filter (fun r -> row |> List.filter (fun s -> s |> List.contains r) |> List.length = 1)
        if marks.Length = 1 then
            {c with Value=PencilMark([marks.Head])}
        else if marks.Length > 1 then
            failwithf "cell %O failed due to having marks %O" c marks
        else
            c

    let updateCell g s c =
        updateCell (fun g s c m -> getSquare s m) g s c 
        |> updateCell (fun g s c m -> getColumn g s c m) g s 
        |> updateCell (fun g s c m -> getRow g s c m) g s
    
    grid |> updateCells (fun _ _ c -> match c.Value with | PencilMark _ -> true | _ -> false) updateCell


let pencil grid =
    grid |> updateCells (fun g s c -> match c.Value with | Empty -> true | PencilMark _ -> true | _ -> false) pencilCell |> eliminate
 




let convertPencilToMarks grid =

    let convert g s c =
        
        match c.Value with 
        | PencilMark v when v.Length = 1 -> 
            {c with Value = Mark(v.Head)} 
        | _ -> c

    grid |> updateCells (fun g s c -> true) convert
   

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