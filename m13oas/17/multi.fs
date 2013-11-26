let main list1 list2 =
    
    let rec digit l =
        match l with
        |h::t -> h < 10 && h > -1 && digit t
        |[] -> true
    
    if (List.head list1 = 1 || list1.Head = -1) && (List.head list2 = 1 || list2.Head = -1) && 
        digit list1.Tail && digit list2.Tail && list1.Length > 1 && list2.Length > 1
    then
        
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
                elif s <= 9 && s >= 0  
                then (s)::(addX t1 t2 (s / 10) n1 n2)
                else (s + 10)::(addX t1 t2 (-1) n1 n2)
            |hd::t1, [] -> 
                let s1 = n1 * hd + k
                if s1 > 9
                then (s1 % 10)::(addX t1 [] (s1 / 10) n1 n2)
                elif s1 <= 9 &&  s1 >= 0
                then (s1)::(addX t1 [] (s1 / 10) n1 n2)
                else (s1 + 10)::(addX t1 [] (-1) n1 n2)     
            |[], hd2::t2 ->
                let s2 = n2 * hd2 + k
                if s2 > 9
                then (s2 % 10)::(addX [] t2 (s2 / 10) n1 n2)
                elif s2 <= 9 && s2 >= 0
                then s2::(addX [] t2 (s2 / 10) n1 n2)
                else (s2 + 10)::(addX [] t2 -1 n1 n2)
    
        let rec multiplicate hd l n p =
            if p > 0 
            then 0::multiplicate hd l n (p - 1)
            else
                match l with
                |[] -> [n]
                |hd3::t3 ->
                    let x = hd * hd3 + n
                    if x > 9
                    then (x % 10)::(multiplicate hd t3 (x / 10) 0)
                    else x::(multiplicate hd t3 0 0)
          
        let  rec multi l1 l2 n =
            match l2 with
                |[] -> []
                |hd2::t2 -> addX (multiplicate hd2 l1 0 n) (multi l1 t2 (n + 1)) 0 1 1
    
        let sign n1 n2  =
            if n1 <> n2 
            then -1
            else 1        
   
        sign list1.Head list2.Head::(multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev |> cutting)
    else failwith "error number format"      
     
printfn "res = %A" (main [1; 9; 9; 9] [1; 1; 1;])
printfn "res2 = %A" (main [-1; 1; 2; 3] [-1; 1; 2; 3])