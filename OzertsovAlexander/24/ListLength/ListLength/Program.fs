type List =
| Lst of int * List
| Empty

let main list =
    let rec len list i =
        match list with
            | Lst (int, List) -> len List (i+1)
            | Empty -> i
    len list 0

printfn "Length = %A" (main (Lst (4, Lst (4, Lst (34, Empty))))) 
