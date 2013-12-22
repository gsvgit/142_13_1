module Addit

open MyT
open Length

let main lst1 lst2 =    

    let rec rev lst =

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

    let findHd x =
        match x with 
        | Lst (hd, tl) -> hd
        | Empty -> 0

    let findTl x =
        match x with 
        | Lst (hd, tl) -> tl
        | Empty -> Empty

    let rec matchLst lst1 lst2 =  
        match lst1, lst2 with
        | Empty, Empty -> false
        | Lst (hd1, tl1), Lst (hd2, tl2) ->  hd1 > hd2 || matchLst tl1 tl2
        | _ -> failwith "wrong input"     
                  
    let checkSign l1 l2  = 
        findHd l1 = findHd l2 
        || findHd l1 = 1 && Length.lstLength l1 > Length.lstLength l2
        || findHd l2 = 1 && Length.lstLength l2 > Length.lstLength l1
        || if Length.lstLength l1 = Length.lstLength l2 && findHd l1 = 1
           then matchLst (findTl l1) (findTl l2)
           elif Length.lstLength l1 = Length.lstLength l2 && findHd l2 = 1
           then matchLst (findTl l2) (findTl l1)
           else false
           
    let rec cutNull lst =
        match lst with
        | Empty -> Lst (0, Empty) 
        | Lst (hd, tl) ->
             if hd = 0
             then cutNull tl
             else Lst (hd, tl)
    
    let rec checkDigit lst =
        match lst with
        | Empty -> true
        | Lst (hd, tl) -> hd <= 9 && checkDigit tl 
                                   
    let checkPosSign = checkSign lst1 lst2
   
    let rec sum lst1 lst2 z x c =  
        match lst1, lst2 with   
        | Lst (hd, tl), Lst (0, Empty)  
        | Lst (0, Empty), Lst (hd, tl) -> Lst (hd, tl)
        | Empty, Empty-> 
            if z = 1 
            then Lst (1, Empty)
            else Empty                        
        | Lst (hd1, tl1), Empty -> 
            let hd1s = hd1 * x + z 
            if hd1s > 9 
            then Lst (hd1s - 10, sum tl1 Empty 1 x c)
            elif hd1s < 0 
            then Lst (hd1s + 10, sum tl1 Empty -1 x c)
            else Lst (hd1s, tl1)      
        | Empty, Lst (hd2, tl2) -> 
            let hd2s = hd2 * c + z
            if hd2s > 9 
            then Lst (hd2s - 10, sum tl2 Empty 1 x c)
            elif hd2s < 0 
            then Lst (hd2s + 10, sum tl2 Empty -1 x c)
            else Lst (hd2s, tl2)         
        | Lst (hd1, tl1), Lst (hd2, tl2) -> 
            let hdSum = hd1 * x + hd2 * c + z  
            if hdSum >= 10  
            then Lst (hdSum - 10, sum tl1 tl2 1 x c)                                 
            elif hdSum < 0 
            then Lst (hdSum + 10, sum tl1 tl2 -1 x c)                                
            else Lst (hdSum, sum tl1 tl2 0 x c)    
        
    let lst1Rev = rev (findTl lst1)
    let lst2Rev = rev (findTl lst2)
    
    if checkDigit lst1 && checkDigit lst2
    then
        if checkPosSign && findHd lst1 = -1 && findHd lst2 = -1
        then Lst (-1, (sum lst1Rev lst2Rev 0 1 1) |> rev |> cutNull)
        elif checkPosSign && findHd lst1 = 1 && findHd lst2 = -1 
             || checkPosSign && findHd lst1 = -1 && findHd lst2 = 1
        then Lst (1, (sum lst1Rev lst2Rev 0 (findHd lst1) (findHd lst2)) |> rev |> cutNull)               
        elif findHd lst1 = 1 && findHd lst2 = -1  || findHd lst1 = -1 && findHd lst2 = 1            
        then Lst (-1, (sum lst2Rev lst1Rev 0 (findHd lst1) (findHd lst2)) |> rev |> cutNull)                 
        else Lst (1, (sum lst1Rev lst2Rev 0 1 1) |> rev |> cutNull)
    else failwith "Digit can't be more than 9)"    
    
main (Lst (1, Lst (8, Lst (8, Lst (1, Empty))))) (Lst (1, Lst (1, Empty))) |> printfn "%A"     
main (Lst (-1, Lst (2, Lst (2, Lst (2, Empty))))) (Lst (1, Lst (2, Lst (2, Lst (5, Empty))))) |> printfn "%A"
main (Lst (1, Lst (5, Empty))) (Lst (-1, Lst (7, Lst (1, Empty)))) |> printfn "%A"
main (Lst (-1, Lst (1, Lst (2, Empty)))) (Lst (1, Lst (1, Empty))) |> printfn "%A"
main (Lst (-1, Lst (10, Lst (5, Empty)))) (Lst (1, Lst (7, Empty))) |> printfn "%A"
