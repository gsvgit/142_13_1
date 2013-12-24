module Multiplication

open reverse 
open typeList 
open length

let main lst lst2 = 
    let rec check lst = 
        match lst with
        | Lst (int, myList) ->
                 if int < 10 
                 then check myList 
                 else false
        | Empty -> true  

    let rec f4 lst = 
            match lst with 
            | Lst (int, myList) -> 
                if int = 0 
                then f4 myList 
                else Lst (int, myList) 
            | Empty -> Lst (0, Empty) 
            
    let t (lst: myList) = 
            match lst with 
            | Lst (hd, tl) -> tl 
            | Empty -> Empty

    let h (lst: myList) = 
        match lst with
        | Lst (hd, tl) -> hd
        | Empty -> 0

    let lstTail = t lst
    let lst2Tail = t lst2
    let lstHead = h lst 
    let lst2Head = h lst2 
    let tailRev = f lstTail
    let tail2Rev = f lst2Tail
    
    if (lengthList lst < 2 || lengthList lst2 < 2) && 
        (abs (h lst) <> 1 || abs (h lst2) <> 1) &&
        (not (check lstTail && check lst2Tail))
    then failwith "error. incorrect input "
    else 
        let sum = ref 0 
        let rec add lst lst2 x s s2 =
            match lst, lst2 with
            | Empty, Empty -> 
                if x = 0 
                then Empty
                else Lst (x, Empty)
            | Lst (int, myList1), Lst (int2, myList2) ->                
                sum := int * s + int2 * s2 + x 
                if !sum > 9 
                then Lst (!sum % 10, add myList1 myList2 (!sum / 10) s s2) 
                else Lst (!sum, add myList1 myList2 (!sum / 10) s s2)                                   
            | Lst (int, myList), Empty
            | Empty, Lst (int, myList) ->                 
                sum := int + x 
                if !sum > 9 
                then Lst (!sum % 10, add myList Empty (!sum / 10) s s2)
                else Lst (!sum, add myList Empty (!sum / 10) s s2)
        let m = ref 0
        let rec multi hd lst x n =
            if n > 0
            then Lst (0, multi hd lst x (n - 1))
            else
                match lst with
                | Empty ->
                    if x > 0 
                    then Lst (x, Empty)
                    else Lst (0, Empty) 
                | Lst (int, myList) ->                   
                    m := hd * int + x
                    if !m > 9 
                    then Lst (!m % 10, multi hd myList (!m / 10) 0)
                    else Lst (!m, multi hd myList 0 0)

        let rec mul lst lst2 d =
            match lst2 with
            | Empty -> Empty
            | Lst (int, myList) ->
                add (multi int lst 0 d) (mul lst myList (d + 1) ) 0 1 1 

        if lstHead = lst2Head
        then Lst (1, mul tailRev tail2Rev 0 |> f |> f4)
        else Lst (-1 ,mul tailRev tail2Rev 0 |> f |> f4)

main (Lst (1, Lst (1, Lst (0, Lst (1, Empty))))) (Lst (1, Lst (1, Lst (0, Empty)))) |> printfn "%A"

main (Lst (1, Lst (2, Empty))) (Lst (1, Lst (1, Empty))) |> printfn "%A"