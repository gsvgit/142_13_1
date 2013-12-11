module ListType
type List =
    | Lst of int * List
    | Empty
let list = Lst(5, Lst(13, Lst(22, Empty)))
printfn "%A" list
