module GridGrid
open Grid
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
        for r in 0..2 -> gridSect (c,r) [
            for ci in 0..2 do
            for ri in 0..2 -> 
                {Column=ci;Row=ri;Value=Empty}
        ]]

let private addMark (x,y,v) grid =
    let sectX,sectY,lx,ly = getLocalCoord (x,y)
    [
        for sect in grid do
        if sect.Column = sectX && sect.Row = sectY then
            yield {sect with Cells = [for cell in sect.Cells do 
                                        if cell.Column = lx && cell.Row = ly then 
                                            yield {cell with Value=Mark(v)}
                                        else yield cell]}
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

