module multiplication

open typeList

let main list1 list2 = 

    let rec deletingZero list =
        match list with
        | Lst (0, Empty) -> Lst (0, Empty)
        | Empty -> Empty
        | Lst (hd, myList) ->
            if hd = 0
            then deletingZero myList
            else Lst (hd, myList)
              
    let rec TwoValuedNumber lst =
        match lst with
        | Lst (hd, myList) -> hd < 10 && TwoValuedNumber myList
        | Empty -> true

    if typeList.length list1 < 2 || typeList.length list2 < 2 
    then failwith "Error. The number is incorrectly set"
    if abs(head list1) <> 1 || abs(head list2) <> 1
    then failwith "Error. the sign can be set only 1 or - 1"
    elif not (TwoValuedNumber (tail list1) && TwoValuedNumber (tail list2))
    then failwith "Error. The digit can't be two-valued number"
    else                                            
               
        let rec addition lst1 lst2 excess sign1 sign2 =
            match lst1, lst2 with
            | Empty, Empty -> 
                if excess = 0 
                then Empty
                else Lst (excess, Empty) 
            | Lst (hd1, myList1), Lst (hd2, myList2) ->                
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then Lst (sum1 % 10, addition myList1 myList2 (sum1 / 10) sign1 sign2) 
                else Lst (sum1, addition  myList1 myList2 (sum1 / 10) sign1 sign2)                                   
            | Lst (hd1, myList1), Empty 
            | Empty, Lst (hd1, myList1) ->                 
                let sum2 = hd1 * 1 + excess 
                if sum2 > 9 
                then Lst (sum2 % 10, addition myList1 Empty (sum2 / 10) sign1 sign2)               
                else Lst (sum2, addition myList1 Empty (sum2 / 10) sign1 sign2)

        let rec multiplication hd list ecx discharge =
            if discharge > 0
            then Lst (0, multiplication hd list ecx (discharge - 1))
            else
                match list with
                | Empty ->
                    if ecx > 0 
                    then Lst (ecx, Empty)
                    else Empty
                | Lst (hd3, myList) ->                   
                    let mul = hd * hd3 + ecx
                    if mul > 9 
                    then Lst (mul % 10, multiplication hd myList (mul / 10) 0) 
                    else Lst (mul, multiplication hd myList 0 0)

        let rec multi list1 list2 disch =
            match list2 with
            |Empty -> Empty
            |Lst (hd4, myList) ->
                addition (multiplication hd4 list1 0 disch) (multi list1 myList (disch + 1) ) 0 1 1 

        if head list1 = head list2
        then Lst (1, (multi (tail list1 |> rev) (tail list2 |> rev) 0 |> rev |> deletingZero))
        else Lst (-1, (multi (tail list1 |> rev) (tail list2 |> rev) 0 |> rev |> deletingZero))

printfn "1. = %A" (main (Lst (1, Lst (9, Lst (9, Lst (0, Empty))))) (Lst (1, Lst (0, Empty))))
printfn "2. = %A" (main (Lst (-1, Lst (1, Lst (2, Lst (3, Empty))))) (Lst (-1, Lst (1, Lst (2, Lst (3, Empty)))))) 
printfn "3. = %A" (main (Lst (1, Lst (2, Lst (2, Lst (3, Empty))))) (Lst (-1, Lst (1, Lst (2, Lst (3, Empty))))))    
printfn "4. = %A" (main Empty (Lst (-1, Lst (1, Lst (2, Lst (3, Empty))))))        
