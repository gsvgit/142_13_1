module Length

open MyT

let rec lstLength lst = 
    match lst with 
    | Lst (int, List) -> lstLength List + 1
    | Empty -> 0

printfn "%A" (lstLength (Lst (124, Lst (24, Empty))))
printfn "%A" (lstLength (Empty))