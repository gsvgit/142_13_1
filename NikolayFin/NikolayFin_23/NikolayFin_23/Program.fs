module typeList

type MyList =
    | Lst of int * MyList
    | Empty

let myList1 = Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))

printfn "%A" myList1