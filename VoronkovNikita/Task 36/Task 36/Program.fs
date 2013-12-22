open myType

let d = Lst (1, Lst (2, Lst (12, Lst (0, Lst (-3, Lst (23, Empty))))))

let main (lst: List) = 

    let rec filterLess x lst = 
        match lst with
        | Lst (h, t) -> if h <= x
                        then Lst (h, filterLess x t)
                        else filterLess x t
        | Empty -> Empty

    let rec filterMore x lst = 
        match lst with
        | Lst (h, t) -> if h > x
                        then Lst (h, filterMore x t)
                        else filterMore x t
        | Empty -> Empty

    let rec connect l1 l2 = 
        match l1 with
        | Lst (h, t) -> Lst (h, connect t l2)
        | Empty -> l2
    
    let rec qsort (m: List) = 
        match m with
        | Empty -> Empty
        | Lst (h, t) -> let s = filterLess h t
                        let l = filterMore h t
                        connect (connect (qsort s) (Lst (h, Empty))) (qsort l)
    
    qsort lst

main d |> printfn "%A"
main (Lst (1, Lst (1, Lst (1, Empty)))) |> printfn "%A"