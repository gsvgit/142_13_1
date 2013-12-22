module addition

open typeList

let main list1 list2 =    

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

        let rec comprassion list1 list2 =
            if typeList.length list1 > typeList.length list2
            then 1
            elif typeList.length list2 > typeList.length list1
            then 2
            else

            match list1, list2 with
            | Empty, Empty -> 0
            | Lst (hd1, myList1), Lst (hd2, myList2) ->
                if hd1 > hd2
                then 1
                elif hd2 > hd1
                then 2
                else comprassion myList1 myList2
            | Lst (hd1, myList1), Empty -> 1
            | Empty, Lst (hd2, myList2) -> 2            
                                                        
        let rec addition list1 list2 excess sign1 sign2 =
            match list1, list2 with
            | Empty, Empty -> 
                if excess = 0 
                then Empty
                else Lst (excess, Empty)
            | Lst (hd1, myList1), Lst (hd2, myList2) ->                 
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then Lst (sum1 % 10, addition myList1 myList2 (sum1 / 10) sign1 sign2)
                elif sum1 >= 0 && sum1 < 10
                then Lst (sum1, addition myList1 myList2 (sum1 / 10) sign1 sign2)                                                           
                else Lst (10 + sum1, addition myList1 myList2 -1 sign1 sign2)                                   
            | Lst (hd1, myList1), Empty 
            | Empty, Lst (hd1, myList1) ->                 
                let sum2 = hd1 * 1 + excess 
                if sum2 > 9 
                then Lst (sum2 % 10, addition  myList1 Empty (sum2 / 10) sign1 sign2)               
                else Lst (sum2, addition myList1 Empty (sum2 / 10) sign1 sign2)    
                            
        let rec deletingZero list =
            match list with
            | Lst (0, Empty) -> Lst (0, Empty)
            | Empty -> Empty
            | Lst (hd, myList) ->
                if hd = 0
                then deletingZero myList
                else Lst (hd, myList)         
                                   
        let compr = comprassion (tail list1) (tail list2)      
                  
        if head list1 =  head list2
        then Lst (head list1, addition (list1 |> tail |> rev) (list2 |> tail |> rev) 0 1 1 |> rev |> deletingZero)
        elif head list1 = 1 && head list2 = -1 && compr = 1
        then Lst (head list1, addition (list1 |> tail |> rev) (list2 |> tail |> rev) 0 1 -1 |> rev |> deletingZero)
        elif head list1 = 1 && head list2 = -1 && compr = 2
        then Lst (head list2, addition (list1 |> tail |> rev) (list2 |> tail |> rev) 0 -1 1 |> rev |> deletingZero)
        elif head list1 = -1 && head list2 = 1 && compr = 1
        then Lst (head list1, addition (list1 |> tail |> rev) (list2 |> tail |> rev) 0 1 -1 |> rev |> deletingZero)
        elif head list1 = -1 && head list2 = 1 && compr = 2
        then Lst (head list2, addition (list1 |> tail |> rev) (list2 |> tail |> rev) 0 -1 1 |> rev |> deletingZero)
        else Lst (1, Lst (0, Empty))            

printfn "1. = %A" (main (Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))) (Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))))
printfn "2. = %A" (main (Lst (-1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))) (Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))))
printfn "3. = %A" (main (Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))) (Lst (-1, Lst (3, Lst (4, Lst (5, Empty))))))
printfn "4. = %A" (main (Lst (1, Lst (3, Lst (4, Lst (5, Empty))))) (Lst (-1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))))
printfn "5. = %A" (main (Lst (-1, Lst (3, Lst (4, Lst (5, Empty))))) (Lst (1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))))
printfn "6. = %A" (main (Lst (-1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))) (Lst (1, Lst (3, Lst (4, Lst (5, Empty))))))
printfn "7. = %A" (main (Lst (-1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))) (Lst (-1, Lst (2, Lst (3, Lst (4, Lst (5, Empty)))))))