module SampleGrid 
open Grid
open GridGrid

let samGrid = create [
    (0,2,5)
    (2,0,1)
    (3,0,2)
    (3,1,6)
    (5,0,3)
    (5,1,7)
    (6,0,4)
    (8,2,3)
    (0,3,3)
    (1,3,7)
    (0,5,6)
    (1,5,2)
    (7,3,8)
    (8,3,1)
    (7,5,3)
    (8,5,7)
    (0,6,1)
    (2,8,6)
    (3,7,8)
    (3,8,4)
    (5,7,5)
    (5,8,2)
    (8,6,8)
    (6,8,5)
]

let uniqueCandidate = create [
    (2,0,4)
    (1,4,4)
    (0,6,5)
    (5,8,4)
]

let sampleGrids = [
    ("Sam's Grid", samGrid)
    ("Unique Candidate", uniqueCandidate)
]