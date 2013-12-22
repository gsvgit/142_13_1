module addition

open myType

let main list1 list2 =    
   
    let rec TwoValuedNumber lst =
        match lst with
        | List (hd, myList) -> hd < 10 && TwoValuedNumber myList
        | Empty -> true
    
    if length.main list1 < 2 || length.main list2 < 2
    then failwith "Error. The number is incorrectly set"
    if abs (myType.myListHead list1) <> 1 || abs (myType.myListHead list2) <> 1
    then failwith "Error. the sign can be set only 1 or - 1"
    elif not (TwoValuedNumber (myType.myListTail list1) && TwoValuedNumber (myType.myListTail list2))
    then failwith "Error. The digit can't be two-valued number"
    else

        let rec comprassion list1 list2 =
            if length.main list1 > length.main list2
            then 1
            elif length.main list2 > length.main list1
            then 2
            else

            match list1, list2 with
            | Empty, Empty -> 0
            | List (hd1, myList1), List (hd2, myList2) ->
                if hd1 > hd2
                then 1
                elif hd2 > hd1
                then 2
                else comprassion myList1 myList2
            | List (hd1, myList1), Empty -> 1
            | Empty, List (hd2, myList2) -> 2            
                                                        
        let rec addition list1 list2 excess sign1 sign2 =
            match list1, list2 with
            | Empty, Empty -> 
                if excess = 0 
                then Empty
                else List (excess, Empty)
            | List (hd1, myList1), List (hd2, myList2) ->                 
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then List (sum1 % 10, addition myList1 myList2 (sum1 / 10) sign1 sign2)
                elif sum1 >= 0 && sum1 < 10
                then List (sum1, addition myList1 myList2 (sum1 / 10) sign1 sign2)                                                           
                else List (10 + sum1, addition myList1 myList2 -1 sign1 sign2)                                   
            | List (hd1, myList1), Empty 
            | Empty, List (hd1, myList1) ->                 
                let sum2 = hd1 * 1 + excess 
                if sum2 > 9 
                then List (sum2 % 10, addition  myList1 Empty (sum2 / 10) sign1 sign2)               
                else List (sum2, addition myList1 Empty (sum2 / 10) sign1 sign2)    
                            
        let rec delzero lst =
            match lst with
            | List (0, Empty) -> List (0, Empty)
            | Empty -> Empty
            | List (hd, myList) ->
                if hd = 0
                then delzero myList
                else List (hd, myList)  
        
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
                                   
        let compr = comprassion (myType.myListTail list1) (myType.myListTail list2)      
                  
        if myType.myListHead list1 =  myType.myListHead list2
        then List (myType.myListHead list1, addition (list1 |> myType.myListTail |> reverse) (list2 |> myType.myListTail |> reverse) 0 1 1 |> reverse |> delzero)
        elif myType.myListHead list1 = 1 && myType.myListHead list2 = -1 && compr = 1
        then List (myType.myListHead list1, addition (list1 |> myType.myListTail |> reverse) (list2 |> myType.myListTail |> reverse) 0 1 -1 |> reverse |> delzero)
        elif myType.myListHead list1 = 1 && myType.myListHead list2 = -1 && compr = 2
        then List (myType.myListHead list2, addition (list1 |> myType.myListTail |> reverse) (list2 |> myType.myListTail |> reverse) 0 -1 1 |> reverse |> delzero)
        elif myType.myListHead list1 = -1 && myType.myListHead list2 = 1 && compr = 1
        then List (myType.myListHead list1, addition (list1 |> myType.myListTail |> reverse) (list2 |> myType.myListTail |> reverse) 0 1 -1 |> reverse |> delzero)
        elif myType.myListHead list1 = -1 && myType.myListHead list2 = 1 && compr = 2
        then List (myType.myListHead list2, addition (list1 |> myType.myListTail |> reverse) (list2 |> myType.myListTail |> reverse) 0 -1 1 |> reverse |> delzero)
        else List (1, List (0, Empty))            

printfn "1 + 0= %A" (main (List (1, List (1,Empty)))  (List (1, List (0,Empty))))
printfn "-723 + 3859 = %A" (main (List (-1, List (7, List (2, List (3, Empty))))) (List (1, List (3, List (8, List (5, List (9, Empty)))))))
printfn "1489 + 8888 = %A" (main (List (1, List (1, List (4, List (8, List (9, Empty)))))) (List (1, List (8, List (8, List (8, List (8, Empty)))))))
printfn "8684 + (-4247) = %A" (main (List (1, List (8, List (6, List (8, List (4, Empty)))))) (List (-1, List (4, List (2, List (4, List (7, Empty)))))))
printfn "= %A" (main (List (1, Empty))  (List (1, Empty)))