module Addition
let main (list1:List< _ >) (list2:List< _ >) = 
    let rec TwoValuedNumber lst =
            match lst with
            |hd::tl -> hd < 10 && TwoValuedNumber tl
            |[] -> true
    if list1.Length < 2 || list2.Length < 2 
    then failwith "Error. The number is incorrectly set"
    if abs(List.head list1) <> 1 || abs(List.head list2) <> 1
    then failwith "Error. the sign can be set only 1 or - 1"
    elif not (TwoValuedNumber list1.Tail && TwoValuedNumber list2.Tail)
    then failwith "Error. The digit can't be two-valued number"
    else
        let rec comprassion l1 l2 =
            if List.length l1 > List.length l2
            then 1
            elif List.length l2 > List.length l1
            then 2
            else
            match l1, l2 with
            |[], [] -> 0
            |hd1::tl1, hd2::tl2 ->
                if hd1 > hd2
                then 1
                elif hd2 > hd1
                then 2
                else comprassion tl1 tl2
            |hd1::t1, [] -> 1
            |[], hd2::t2 -> 2                                                        
        let rec addition lst1 lst2 excess sign1 sign2 =
            match lst1, lst2 with
            |[], [] -> 
                if excess = 0 
                then []
                else [excess]
            |hd1::tl1, hd2::tl2 ->                 
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then sum1 % 10::addition tl1 tl2 (sum1 / 10) sign1 sign2 
                elif sum1 >= 0 && sum1 < 10
                then sum1::addition tl1 tl2 (sum1 / 10) sign1 sign2                                        
                else 10 + sum1::addition tl1 tl2 -1 sign1 sign2                 
            |hd1::tl1,[] 
            |[], hd1::tl1 ->                 
                let sum2 = hd1 * 1 + excess 
                if sum2 > 9 
                then sum2 % 10::addition tl1 [] (sum2 / 10) sign1 sign2
                else sum2::addition tl1 [] (sum2 / 10) sign1 sign2
        let rec deletingZero list =
            match list with
            |[] -> [0]
            |hd::tl ->
                if hd = 0
                then deletingZero tl
                else hd::tl                            
        let compr = comprassion (list1.Tail) (list2.Tail)                
        if list1.Head =  list2.Head
        then list1.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 1 1 |> List.rev |> deletingZero)
        elif list1.Head = 1 && list2.Head = -1 && compr = 1
        then list1.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 1 -1 |> List.rev |> deletingZero)
        elif list1.Head = 1 && list2.Head = -1 && compr = 2
        then list2.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 -1 1 |> List.rev |> deletingZero)
        elif list1.Head = -1 && list2.Head = 1 && compr = 1
        then list1.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 1 -1 |> List.rev |> deletingZero)
        elif list1.Head = -1 && list2.Head = 1 && compr = 2
        then list1.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 -1 1|> List.rev |> deletingZero)
        else 1::[0]              
printfn "1. = %A" (main [1; 9; 9; 9] [1; 9; 9; 0])
printfn "2. = %A" (main [-1; 6; 6; 0] [-1; 5; 7; 9])
printfn "3. = %A" (main [1; 6; 6; 0] [-1; 6; 6; 0])
printfn "4. = %A" (main [-1; 9; 8; 7] [1; 6; 6; 0])
printfn "5. = %A" (main [1; 9; 8; 7] [-1; 6; 6; 0])  
printfn "6. = %A" (main [1; 6; 6; 0] [-1; 9; 8; 7]) 
printfn "7. = %A" (main [-1; 6; 6; 1] [1; 9; 8; 7])     
printfn "8. = %A" (main [1; 6] [-1; 1; 6; 0]) 
printfn "9. = %A" (main [1; 16] [-1; 1; 6; 0]) 