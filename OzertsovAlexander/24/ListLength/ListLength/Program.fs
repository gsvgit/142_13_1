open MyType
let main collect =
    let rec len collect i =
        match collect with
            | Lst (int, MyList) -> len MyList (i + 1)
            | Empty -> i
    len collect 0

printfn "Length = %A" (main (Lst (4, Lst (4, Lst (34, Empty))))) 
