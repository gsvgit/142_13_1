module Multi

open MyT

let main (lst1: MyT.List) (lst2: MyT.List) =    
    let rec rev (lst: MyT.List) =
        let rec ret lst =
            match lst with
            | Lst (hd, Empty) -> hd
            | Lst (hd, tl) -> ret tl
            | Empty -> 0
        let rec cut lst =
            match lst with 
            | Lst (hd, Empty) -> Empty
            | Lst (hd, tl) -> Lst (hd, cut tl)
            | Empty -> Empty
        match lst with
        | Empty -> Empty
        | Lst (hd, tl) -> Lst (ret lst, rev (cut lst))

    let findHd (x: MyT.List) = 
        match x with
        | Lst (hd, tl) -> hd
        | Empty -> 0

    let findTl (x: MyT.List) = 
        match x with
        | Lst (hd, tl) -> tl
        | Empty -> Empty 
    
    let rec multiDigit lst hd s =
        match lst with
        | Empty -> 
            if s > 0
            then Lst (s, Empty)
            else Empty
        | Lst(hd1, tl1) -> 
            let hdMult = hd1 * hd + s
            if hdMult > 9
            then Lst (hdMult % 10, multiDigit tl1 hd (hdMult / 10)) 
            else Lst (hdMult, multiDigit tl1 hd 0) 
                                                            
    let rec sum lst1 lst2 s =          
        match lst1, lst2 with   
        | Lst (hd, tl), Lst (0, Empty) -> Lst (hd, tl)  
        | Lst (0, Empty), Lst(hd, tl) -> Lst (hd, tl)
        | Empty, Empty -> 
            if s = 1
            then Lst (1, Empty)
            else Empty                
        | Lst (hd1, Empty), Lst (hd2, Empty) -> 
            let hdSum = hd1 + hd2 + s 
            if hdSum >= 10 
            then Lst (hdSum - 10, Lst(1, Empty))            
            else Lst (hdSum,  Empty) 
        |  Lst (hd, tl), Empty ->
            let hs = hd + s
            if hs > 9
            then Lst ((hs - 10), sum tl Empty 1)
            else Lst (hs, tl)
        | Empty, Lst (hd, tl) ->    
            let hs = hd + s
            if hs > 9
            then Lst ((hs - 10), sum Empty tl 1)
            else Lst (hs, tl)                             
        | Lst (hd1, tl1), Lst (hd2, tl2) -> 
            let hdSum = hd1 + hd2 + s  
            if hdSum >= 10  
            then Lst (hdSum - 10, sum tl1 tl2 1)            
            else Lst (hdSum, sum tl1 tl2 0) 
    
    let rec addEndNull lst z =
        if z > 0
        then Lst (0, addEndNull lst (z - 1))
        else lst           

    let rec multiList lst1 lst2 n =       
        match lst2 with        
        | Empty -> Empty        
        | Lst (hd1, tl1) ->  sum (addEndNull (multiDigit lst1 hd1 0) n) (multiList lst1 tl1 (n + 1)) 0 
                                                                     
    let checkSign a b =
        if a = b
        then 1
        else -1

    let rec checkDigit lst =
        match lst with
        | Empty -> true
        | Lst (hd, tl) -> hd <= 9 && checkDigit tl 
    
    if lst1 = Empty || lst2 = Empty || lst1 = Lst (1, Empty) || lst2 = Lst (1, Empty) || lst1 = Lst (-1, Empty) || lst2 = Lst (-1, Empty)
    then failwith "Empty list"
    elif checkDigit lst1 && checkDigit lst2
    then         
        if findHd lst1 = 0 || findHd lst2 = 0
        then Lst (0, Empty)
        else Lst (checkSign (findHd lst1) (findHd lst2), rev (multiList (rev (findTl lst1)) (rev (findTl lst2)) 0))                   
    else failwith "Incorrect input list"
          
main (Lst (1, Lst (8, Lst (8, Lst (1, Empty))))) (Lst (1, Lst (1, Empty))) |> printfn "%A"     
main (Lst (-1, Lst (2, Lst (2, Lst (2, Empty))))) (Lst (1, Lst (2, Lst (2, Lst(5, Empty))))) |> printfn "%A"
main (Lst (-1, Lst (5, Empty))) (Lst (-1, Lst(7, Empty))) |> printfn "%A"
main (Lst (-1, Lst (1, Lst (20, Empty)))) (Lst (1, Lst (1, Empty))) |> printfn "%A"