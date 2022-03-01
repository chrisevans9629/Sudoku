module Grid

type CellValue = 
    | PencilMark of int list 
    | PermaMark of int
    | Mark of int
    | Empty
type CellModel = {Column:int; Row:int;Value:CellValue }

let mark (c,r) v = {Column=c;Row=r;Value=Mark(v)}
let pencil (c,r) v = {Column=c;Row=r;Value=PencilMark(v)}