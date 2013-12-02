type List = 
    | Lst of int * List
    | Empty
let length = ref 0
let rec main x = 
    match x with
    | Lst (a, n1) -> 
        length := !length + 1
        main n1
    | Empty -> !length
let myList = Lst (1, Lst (4, Lst (9, Empty)))
main myList |> printfn "%A"
