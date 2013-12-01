open TypeList

let rec lstLength (lst: MyPerfectList) = 
    match lst with
    | Lst (int, MyPerfectList) -> lstLength MyPerfectList + 1
    | Empty -> 0

printfn "%A" (lstLength (Lst (42, Lst (32, Empty))))
printfn "%A" (lstLength (Empty))