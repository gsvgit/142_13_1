open typeList

let rec main (list:MyList) i =
    match list with 
    | Lst (int, myList) -> main myList i + 1
    | Empty -> i

let myList1 = Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))

printfn "myList1.Length = %A" (main myList1 0)
