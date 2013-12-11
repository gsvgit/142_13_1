open typeList

let lengthList( x: myList) = 
    let l = ref 0
    let rec count ( x: myList) = 
        match x with
        | Lst(int, myList) -> l := !l + 1
                              count myList
        | Empty -> !l
    count x
    
let lt =  Lst (0, Lst (1, Lst (11, Lst (111, Empty))))
let lt1 = Empty

lengthList lt |> printfn "%A"
lengthList lt1|> printfn "%A"
