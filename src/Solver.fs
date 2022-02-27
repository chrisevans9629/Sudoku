module Solver
open GridGrid
open Grid

let checkFor = [1..9]

let getRow grid sect cell =
    let _,y = getGlobalCoord sect cell
    [for s in grid do
     for c in s.Cells do
        let _,y2 = getGlobalCoord s c
        match c.Value with
        | Mark v when y2 = y -> yield v
        | _ -> ()
        ]

let getColumn grid sect cell =
    let x,_ = getGlobalCoord sect cell
    [for s in grid do
     for c in s.Cells do
        let x2,_ = getGlobalCoord s c
        match c.Value with
        | Mark v when x = x2 -> yield v
        | _ -> ()
        ]

let getSquare sect =
    [for cell in sect.Cells do
        match cell.Value with
        | Mark v -> yield v
        | _ -> ()]

let pencilCell grid sect cell =
    let unavailable = getColumn grid sect cell
                    |> List.append (getRow grid sect cell)
                    |> List.append (getSquare sect)
    let result = checkFor |> List.filter (fun i -> unavailable |> List.contains i |> not)
    PencilMark result

let pencilCells grid sect =
     [for cell in sect.Cells do
        match cell.Value with
        | Empty ->
            {cell with Value = pencilCell grid sect cell}
        | _ -> cell]

let pencil grid =
    [for sect in grid do
        yield {sect with Cells = pencilCells grid sect}]
    
     
