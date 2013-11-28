type List =
| Lst of int * List
| Empty

let main list =
    let rec len list i =
        match list with
            | Lst (int, List) -> len List (i+1)
            | Empty -> printfn "length = %A" i
    len list 0

main (Lst (4, Lst (4, Lst (34, Empty)))) 
