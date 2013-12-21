module MyT

type List = 
    | Lst of int * List
    | Empty

printfn "%A" (Lst(45, Lst(21, Empty)))
printfn "%A" (Lst(25, Lst(25, Lst(24, Empty))))
