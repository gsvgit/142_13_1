module Add

open myType
open Length

let main lst1 lst2 = 
    let rec reverse lst =
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
    
    let findingTail x = 
        match x with
        | Lst (h, t) -> t
        | Empty -> Empty 
    
    let findingHead x = 
        match x with
        | Lst (h, t) -> h
        | Empty -> 0
    
    let lst1Rev = reverse (findingTail lst1)
    let lst2Rev = reverse (findingTail lst2)  
    
    let rec matchLst lst1 lst2 =  
        match lst1, lst2 with
        | Empty, Empty -> false
        | Lst (h1, t1), Lst (h2, t2) ->  h1 > h2 || matchLst t1 t2
        | _ -> failwith "Incorrect input"                  
    
    let help l1 l2  = 
        findingHead l1 = findingHead l2 
        || findingHead l1 = 1 && Length.main l1 > Length.main l2
        || findingHead l2 = 1 && Length.main l2 > Length.main l1
        || if Length.main l1 = Length.main l2 && findingHead l1 = 1
           then matchLst (findingTail l1) (findingTail l2)
           elif Length.main l1 = Length.main l2 && findingHead l2 = 1
           then matchLst (findingTail l2) (findingTail l1)
           else false           
    
    let rec deletingZeroes lst =
        match lst with
        | Empty -> Lst (0, Empty)
        | Lst (h, t) ->
             if h = 0
             then deletingZeroes t
             else Lst (h, t)   
    
    let rec checkDigit lst =
        match lst with
        | Empty -> true
        | Lst (h, t) -> h <= 9 && checkDigit t                                    
    
    let res = help lst1 lst2   
    
    let rec sum lst1 lst2 a b c =  
        match lst1, lst2 with   
        | Lst (h, t), Lst (0, Empty)  
        | Lst (0, Empty), Lst (h, t) -> Lst (h, t)
        | Empty, Empty -> 
            if a = 1 
            then Lst (1, Empty) 
            else Empty                        
        | Lst (h1, Empty), Lst (h2, Empty) -> 
            let hd = h1 * b + h2 * c + a 
            if hd >= 10 
            then Lst (hd - 10, (Lst (1, Empty)))
            elif hd < 0 
            then Lst (hd + 10, (Lst (1, Empty)))
            else Lst (hd, Empty)
        | Lst (h1, t1), Empty -> 
            let hd = h1 * b + a 
            if hd > 9 
            then Lst (hd - 10, sum t1 Empty 1 b c)
            elif hd < 0 
            then Lst (hd + 10, sum t1 Empty -1 b c)
            else Lst (hd, t1)          
        | Empty, Lst (h2, t2) -> 
            let hd = h2 * c + a 
            if hd > 9 
            then Lst (hd - 10, sum t2 Empty 1 b c)
            elif hd < 0 
            then Lst (hd + 10, sum t2 Empty -1 b c)
            else Lst (hd, t2)      
        | Lst (h1, t1), Lst (h2, t2) -> 
            let headSum = h1 * b + h2 * c + a  
            if headSum >= 10  
            then Lst (headSum - 10, sum t1 t2 1 b c)                                  
            elif headSum < 0 
            then Lst (headSum + 10, sum t1 t2 -1 b c)                                 
            else Lst (headSum, sum t1 t2 0 b c)                
    
    if checkDigit lst1 && checkDigit lst2
    then
        if res && findingHead lst1 = -1 && findingHead lst2 = -1
        then Lst (-1, deletingZeroes (reverse (sum lst1Rev lst2Rev 0 1 1)))
        elif res && findingHead lst1 = 1 && findingHead lst2 = -1 
             || res && findingHead lst1 = -1 && findingHead lst2 = 1
        then Lst (1, deletingZeroes (reverse (sum lst1Rev lst2Rev 0 (findingHead lst1) (findingHead lst2))))            
        elif findingHead lst1 = 1 && findingHead lst2 = -1  || findingHead lst1 = -1 && findingHead lst2 = 1            
        then Lst (-1, deletingZeroes (reverse (sum lst2Rev lst1Rev 0 (findingHead lst1) (findingHead lst2))))                
        else Lst (1, deletingZeroes (reverse (sum lst1Rev lst2Rev 0 1 1)))
    else failwith "Error! Digit must be from 0 to 9!"      

main (Lst(1, Lst(9, Lst(9, Lst(9, Lst(9, Empty)))))) (Lst (1, Lst(1, Empty))) |> printfn "%A"     
main (Lst(-1, Lst(1, Lst(2, Lst(3, Lst(4, Lst(5, Empty))))))) (Lst (1, Lst(5, Lst(4, Lst(3, Lst(2, Lst(1, Empty))))))) |> printfn "%A"
main (Lst (1, Lst(9, Empty))) (Lst (1, Lst(1, Lst(6, Empty)))) |> printfn "%A"
main (Lst (1, Lst(8, Empty))) (Lst (-1, Lst (9, Empty))) |> printfn "%A"