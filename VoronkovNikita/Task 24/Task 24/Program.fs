open myType

let main lst = 
    let length = ref 0
    let rec help x = 
        match x with
        | Lst (a, n1) -> 
            length := !length + 1
            help n1
        | Empty -> !length
    help lst
let myList = Lst (1, Lst (4, Lst (9, Empty)))
main myList |> printfn "%A"
main Empty |> printfn "%A"
