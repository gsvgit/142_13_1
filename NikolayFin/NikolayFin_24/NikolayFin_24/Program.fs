open typeList

let rec main (list:MyList) =
    match list with 
    | Lst (int, myList) -> main myList + 1
    | Empty -> 0

let myList1 = Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))

printfn "myList1.Length = %A" (main myList1)
