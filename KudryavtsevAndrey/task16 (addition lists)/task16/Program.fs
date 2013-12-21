module Addition

let main list1 list2 = 
    
    let rec correctionCheck lst = 
        match lst with 
        | hd::tl -> hd < 10 && hd > -10 && correctionCheck tl 
        | [] -> true
    
    if (List.head list1 = 1 || List.head list1 = -1) &&
       (List.head list2 = 1 || List.head list2 = -1) &&
       correctionCheck list1 = true &&
       correctionCheck list2 = true &&
       list1.Length > 1 && list2.Length > 1
    then 
    
        let rec listMatching lst1 lst2 = 
            if List.length lst1 > List.length lst2
            then 1 
            elif List.length lst1 < List.length lst2
            then 2
            else
                match lst1, lst2 with
                | [], [] -> 0 
                | hd1::tl1, hd2::tl2 -> 
                    if hd1 > hd2 
                    then 1 
                    elif hd1 < hd2 
                    then 2 
                    else listMatching tl1 tl2 
                |hd1::t1, [] -> 1
                |[], hd2::t2 -> 2
                    
        let rec cuttingZeroes lst = 
            match lst with 
            | [] -> [0] 
            | hd::tl -> 
                if hd = 0 
                then cuttingZeroes tl 
                else hd::tl 
                
        let rec addition l1 l2 excess sign1 sign2 = 
            match l1, l2 with 
            | [], [] -> [excess]
            | hd1::tl1, hd2::tl2 -> 
                let sum = hd1 * sign1 + hd2 * sign2 + excess 
                if sum > 9 
                then (sum % 10)::(addition tl1 tl2 (sum / 10) sign1 sign2) 
                elif sum <= 9 && sum >= 0 
                then sum::(addition tl1 tl2 (sum / 10) sign1 sign2)
                else (sum + 10)::(addition tl1 tl2 -1 sign1 sign2) 
            | [], hd1::tl1
            | hd1::tl1, [] -> 
                let sum1 = hd1 * sign1 + excess 
                if sum1 > 9 
                then (sum1 % 10)::(addition tl1 [] (sum1 / 10) sign1 sign2) 
                elif sum1 <= 9 && sum1 >= 0 
                then sum1::(addition tl1 [] (sum1 / 10) sign1 sign2) 
                else 
                (sum1 + 10)::(addition tl1 [] -1 sign1 sign2) 
            
        let ending list = 
            list |> List.rev |> cuttingZeroes 
           
        let x = listMatching list1.Tail list2.Tail 
        if x = 1 && list1.Head > list2.Head 
        then list1.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 list1.Head list2.Head |> ending) 
        elif x = 1 && List.head list1 < List.head list2
        then list1.Head::(addition (List.rev list1.Tail) (List.rev list2.Tail) 0 list2.Head list1.Head |> ending)
        elif x = 2 && List.head list1 < List.head list2
        then list2.Head::(addition (List.rev list2.Tail) (List.rev list1.Tail) 0 list2.Head list1.Head |> ending)
        elif (x = 2 && List.head list1 = List.head list2) ||
             (x = 0 && List.head list1 = List.head list2) ||
             (x = 1  && List.head list1 = List.head list2)
        then list2.Head::(addition (List.rev list2.Tail) (List.rev list1.Tail) 0 1 1 |> ending)
        elif x = 2 && List.head list1 > List.head list2
        then list2.Head::(addition (List.rev list2.Tail) (List.rev list1.Tail) 0 list1.Head list2.Head |> ending)
        else 1::[0]
    else failwith "error! wrong numbers format" 
    
printfn "res = %A" (main [1; 1; 1; 1] [-1; 1; 1; 1])
printfn "res2 = %A" (main [-1; 5; 5; 5] [-1; 7; 7; 7])
printfn "res2 = %A" (main [-1; 7; 7; 7] [-1; 5; 5; 5])
printfn "res3 = %A" (main [1; 1; 1;] [-1; 5; 6; 7])
printfn "res4 = %A" (main [1; 2; 3; 4] [1; 0])