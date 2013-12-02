type List = 
    | Lst of int * List
    | Empty

printfn "%A" (Lst(45, Lst(21, Empty)))

