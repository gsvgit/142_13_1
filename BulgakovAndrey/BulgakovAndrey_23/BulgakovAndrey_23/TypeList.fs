module TypeList
type MyList =
    | Lst of int * MyList
    | Empty
let k = Lst(1, Lst(2, Empty))
printfn "%A" k