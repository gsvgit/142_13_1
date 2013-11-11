let main list1 list2 =
    let rec digit l =
        match l with
        |h::t -> h < 10 && h > -10 && digit t
        |[] -> true

    if (List.head list1 = 1 || List.head list1 = -1) && 
       (List.head list2 = 1 || List.head list2 = -1) && 
       List.length list1 > 1 && List. length list2 > 1 && 
       digit list1 && digit list2
    then
        let rec equal l1_1 l2_2 =
            if List.length l1_1 > List.length l2_2 
            then 1
            elif List.length l1_1 < List.length l2_2
            then 2
            else 
                match l1_1, l2_2 with
                |[], [] -> 0
                |hd1::t1, hd2::t2 ->
                    if hd1 > hd2
                    then 1
                    elif hd2 > hd1
                    then 2
                    else equal t1 t2
                |hd1::t1, [] -> 1
                |[], hd2::t2 -> 2
    
        let rec cutting lis =
            match lis with
            |[] -> [0]
            |hd::tl ->
                if hd = 0
                then (cutting tl)
                else hd::tl               
    
        let rec addX l1 l2 k n1 n2= 
            match l1, l2 with
            |[], [] -> [k]
            |hd::t1, hd2::t2 ->
                let s = n1 * hd + n2 * hd2 + k
                if s > 9 
                then (s % 10)::(addX t1 t2 (s / 10) n1 n2)
                elif s < 10 && s >= 0  
                then (s)::(addX t1 t2 (s / 10) n1 n2)
                else (s + 10)::(addX t1 t2 (-1) n1 n2)
            |[], hd::t1 
            |hd::t1, [] -> 
                let s1 = n1 * hd + k
                if s1 > 9
                then (s1 % 10)::(addX t1 [] (s1 / 10) n1 n2)
                elif s1 < 10 &&  s1 >= 0
                then (s1)::(addX t1 [] (s1 / 10) n1 n2)
                else (s1 + 10)::(addX t1 [] (-1) n1 n2)
        
        let m = equal list1.Tail list2.Tail 
        if m = 1 && List.head list1 > List.head list2
        then list1.Head::(addX (List.rev list1.Tail) (List.rev list2.Tail) 0 list1.Head  list2.Head |> List.rev |> cutting)
        elif m = 1 && List.head list1 < List.head list2
        then list1.Head::(addX (List.rev list1.Tail) (List.rev list2.Tail) 0  list2.Head list1.Head|> List.rev |> cutting)
        elif m = 2 && List.head list1 < List.head list2
        then list2.Head::(addX (List.rev list2.Tail) (List.rev list1.Tail) 0 list2.Head  list1.Head |> List.rev |> cutting)
        elif (m = 2 && List.head list1 = List.head list2) ||
             (m = 0 && List.head list1 = List.head list2) ||
             (m = 1  && List.head list1 = List.head list2)
        then list2.Head::(addX (List.rev list2.Tail) (List.rev list1.Tail) 0 1  1 |> List.rev |> cutting)
        elif m = 2 && List.head list1 > List.head list2
        then list2.Head::(addX (List.rev list2.Tail) (List.rev list1.Tail) 0 list1.Head  list2.Head |> List.rev |> cutting)
        else 1::[0]
    else failwith "error number format" 
         
printfn "res = %A" (main [1; 6; 6; 0] [-1; 6; 6; 0])
printfn "res2 = %A" (main [-1; 9; 9; 9] [-1; 8; 8; 8])
printfn "res2 = %A" (main [-1; 8; 8; 8] [-1; 9; 9; 9])
printfn "res3 = %A" (main [1; 6; 6;] [-1; 7; 5; 9])