open ListType

let rec listLength list =
    match list with
        | Lst (int, List) -> listLength List + 1
        | Empty -> 0
printfn "%A" (listLength (Lst(5, Lst(13, Lst(22, Empty)))))
printfn "%A" (listLength (Empty))
