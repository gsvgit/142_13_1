module Multip

open myType

let main (lst1: List) (lst2: List) = 
    
    let rec reverse (lst: List) =
        let rec returnLast lst =
            match lst with
            | Lst (h, Empty) -> h
            | Lst (h, t) -> returnLast t
            | Empty -> 0
        let rec cut lst =
            match lst with 
            | Lst (h, Empty) -> Empty
            | Lst (h, t) -> Lst (h, cut t)
            | Empty -> Empty
        match lst with
        | Empty -> Empty
        | Lst (h, t) -> Lst (returnLast lst, reverse (cut lst))
    
    let findingTail (x: List) = 
        match x with
        | Lst (h, t) -> t
        | Empty -> Empty 
    
    let findingHead (x: List) = 
        match x with
        | Lst (h, t) -> h
        | Empty -> 0

    let check lt = 
        if lt = Lst (1, Lst (0, Empty)) ||
           lt = Lst (-1, Lst (0, Empty)) ||
           lt = Lst (0, Empty)
        then false
        else true

    let rec checkDigit lt = 
        match lt with
        | Lst (h, t) -> if h < 0 || h > 10
                        then 10
                        else checkDigit t
        | Empty -> 0

    if lst1 = Empty || 
       lst2 = Empty ||
       findingTail lst2 = Empty ||
       findingTail lst1 = Empty
    then failwith "Error! You didn't enter at least one number."

    elif findingHead lst1 <> 1 && findingHead lst1 <> -1 || 
       findingHead lst2 <> 1 && findingHead lst2 <> -1
    then failwith "Error! You entered wrong sign (first element in every list). Sign must be equal 1 or -1"
    
    elif checkDigit (findingTail lst1) = 10 ||
         checkDigit (findingTail lst2) = 10
    then failwith "Error! You entered wrong digit. Digit must be from 0 to 9."

    elif check lst1 = false ||
         check lst2 = false
    then Lst (0, Empty)

    else
        let lst1Rev = reverse (findingTail lst1)
        let lst2Rev = reverse (findingTail lst2)
        let rec summing lst1 lst2 s =          
            match lst1, lst2 with   
            | Empty, Empty -> Empty
            | Lst (h1, t1), Empty ->
                let hd = h1 + s
                if s = 1 
                then Lst (hd, t1)            
                else Lst (h1, t1)
            | Empty, Lst (h2, t2) ->    
                let hd = h2 + s
                if s = 1 
                then Lst (hd, t2)            
                else Lst (h2, t2) 
            | Lst (h1, t1), Lst (0, Empty) -> Lst (h1, t1)
            | Lst (0, Empty), Lst (h2, t2) -> Lst (h2, t2)    
            | Lst (h1, Empty), Lst (h2, Empty) -> 
                let hd = h1 + h2 + s 
                if hd >= 10 
                then Lst (hd - 10, Lst (1, Empty))            
                else Lst (hd, Empty)                 
            | Lst (h1, t1), Lst (h2, t2) -> 
                let hd = h1 + h2 + s  
                if hd >= 10  
                then Lst (hd - 10, summing t1 t2 1)            
                else Lst (hd, summing t1 t2 0)
       
        let rec additionZeroes lst n =
            if n > 0
            then Lst (0, additionZeroes lst (n - 1))
            else lst 
                      
        let rec help lst hd m =
            match lst with
            | Empty -> 
                if m > 0
                then Lst (m, Empty)
                else Empty
            | Lst (h, t) -> 
                let hds = h * hd + m
                if hds > 9
                then Lst (hds % 10, help t hd (hds / 10)) 
                else Lst (hds, help t hd 0)

        let rec listsMult lst1 lst2 n =       
            match lst2 with        
            | Empty -> Empty        
            | Lst (h, t) ->  summing (additionZeroes (help lst1 h 0) n) (listsMult lst1 t (n + 1)) 0 
                                                                        
        Lst (findingHead lst1 * findingHead lst2, reverse (listsMult lst1Rev lst2Rev 0))

main (Lst (1, Lst (2, Lst (3, Empty)))) (Lst (1, Lst (2, Lst (3, Empty)))) |> printfn "%A"
main (Lst (1, Lst (3, Lst (2, Lst (1, Empty))))) (Lst (1, Lst (1, Lst (2, Lst (3, Empty))))) |> printfn "%A"
main (Lst (-1, Lst (9, Lst (9, Empty)))) (Lst (-1, Lst (0, Empty))) |> printfn "%A"
main (Lst (1, Lst (2, Empty))) Empty |> printfn "%A"

