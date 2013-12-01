type List = 
    | Lst of int * List
    | Empty
let myList = Lst (1, Lst (4, Lst (9, Lst (16, Lst (25, Empty)))))
myList |> printfn "%A"

