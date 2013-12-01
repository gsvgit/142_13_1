open MyT

let rec lstLength (lst: Tree) = 
    match lst with 
    | Lst (int, Tree) -> lstLength Tree + 1
    | Empty -> 0

printfn "%A" (lstLength (Lst (124, Lst (24, Empty))))
printfn "%A" (lstLength (Empty))