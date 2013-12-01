type Tree = 
    | Lst of int * Tree
    | Empty

printfn "%A" (Lst(45, Lst(21, Empty)))

