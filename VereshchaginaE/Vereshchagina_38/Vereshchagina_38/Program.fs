module multiplication

open myType
open length

let main list1 list2 = 
    
    let rec delzero list =
        match list with
        |List (0, Empty) -> List (0, Empty)
        |Empty -> Empty
        |List (hd, myList) ->
            if hd = 0
            then delzero myList
            else List (hd, myList)
              
    let rec check lst =
        match lst with
        |List (hd, myList) -> hd < 10 && check myList
        |Empty -> true

    let head1 = myType.myListHead list1
    let head2 = myType.myListHead list2

    if length.main list1 < 2 || length.main list2 < 2 
    then failwith "Incorrect number format"
    if abs(myType.myListHead list1) <> 1 || abs(myType.myListHead list2) <> 1
    then failwith "Incorrect sign (can be only 1 or -1)"
    elif not (check (myType.myListTail list1) && check (myType.myListTail list2))
    then failwith "Error. The digit must be in 1..9"
    else                                            
        let rec addition lst1 lst2 oddment sign1 sign2 =
            match lst1, lst2 with
            | List (hd1, myList1), List (hd2, myList2) ->                
                let sum1 = hd1 * sign1 + hd2 * sign2 + oddment 
                if sum1 > 9 
                then List (sum1 % 10, addition myList1 myList2 (sum1 / 10) sign1 sign2) 
                else List (sum1, addition  myList1 myList2 (sum1 / 10) sign1 sign2)                                   
            | List (hd1, myList1), Empty 
            | Empty, List (hd1, myList1) ->                 
                let sum2 = hd1 * 1 + oddment 
                if sum2 > 9 
                then List (sum2 % 10, addition myList1 Empty (sum2 / 10) sign1 sign2)               
                else List (sum2, addition myList1 Empty (sum2 / 10) sign1 sign2)
            | Empty, Empty -> 
                if oddment = 0 
                then Empty
                else List (oddment, Empty) 
            
        let rec multiplication hd list oddmt discharge =
            if discharge > 0
            then List (0, multiplication hd list oddmt (discharge - 1))
            else
                match list with
                | Empty ->
                    if oddmt > 0 
                    then List (oddmt, Empty)
                    else Empty
                | List (hd3, myList) ->                   
                    let mul = hd * hd3 + oddmt
                    if mul > 9 
                    then List (mul % 10, multiplication hd myList (mul / 10) 0) 
                    else List (mul, multiplication hd myList 0 0)
        
        let rec reverse (lst: MyList) =
            let rec returnLast lst =
                match lst with
                | List (hd, Empty) -> hd
                | List (hd, tl) -> returnLast tl
                | Empty -> 0
            let rec cut lst =
                match lst with 
                | List (hd, Empty) -> Empty
                | List (hd, tl) -> List (hd, cut tl)
                | Empty -> Empty
            match lst with
            | Empty -> Empty
            | List (hd, tl) -> List (returnLast lst, reverse (cut lst))
        
        let rec multi list1 list2 disch =
            match list2 with
            |Empty -> Empty
            |List (hd4, myList) ->
                addition (multiplication hd4 list1 0 disch) (multi list1 myList (disch + 1) ) 0 1 1 

        if head1 = head2
        then List (1, (multi (myType.myListTail list1 |> reverse) (myType.myListTail list2 |> reverse) 0 |> reverse |> delzero))
        else List (-1, (multi (myType.myListTail list1 |> reverse) (myType.myListTail list2 |> reverse) 0 |> reverse |> delzero))

printfn "1 * 0 = %A" (main (List (1, List (1, Empty))) (List (1, List (0, Empty))))
printfn "-1 * (-13) = %A" (main (List (-1, List (1, Empty))) (List (-1, List (1, List (3, Empty))))) 
printfn "788 * (-993) = %A" (main (List (1, List (7, List (8, List (8, Empty))))) (List (-1, List (9, List (9, List (3, Empty))))))    
printfn "%A" (main (List (1, Empty)) (List (-1, List (1, Empty)))) 
 
