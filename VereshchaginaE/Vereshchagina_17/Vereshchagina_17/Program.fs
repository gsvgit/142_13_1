module Multiplication

let main (lst1 : List<_>) (lst2 : List<_>) =
     
    let rec addition lst1 lst2 s =
            match lst1, lst2 with
            |hd1 :: tl1, hd2 :: tl2 ->                
                let sum1 = hd1 + hd2 + s 
                if sum1 > 9 
                then sum1 % 10 :: addition tl1 tl2 (sum1 / 10)  
                else sum1 :: addition tl1 tl2 (sum1 / 10)                                     
            |hd1 :: tl1, []
            |[], hd1 :: tl1 ->                 
                let sum2 = hd1 + s 
                if sum2 > 9 
                then sum2 % 10 :: addition tl1 [] (sum2 / 10) 
                else sum2 :: addition tl1 [] (sum2 / 10)
            |[], [] -> 
                if s = 0 
                then []
                else [s]           
        
    let rec mult lst hd m =
        match lst with
        |[] -> 
            if m > 0
            then [m]
            else []
        |hd1 :: tl1 -> 
            let hdMult = hd1 * hd + m
            if hdMult > 9
            then hdMult % 10 :: mult tl1 hd (hdMult / 10) 
            else  hdMult :: mult tl1 hd 0
    
    let rec addzero lst n =
        if n > 0
        then 0 :: addzero lst (n - 1)
        else lst         
    
    let rec multiplication lst1 lst2 n =       
        match lst2 with        
        |[] -> []        
        |hd1 :: tl1 ->  addition (addzero (mult lst1 hd1 0) n) (multiplication lst1 tl1 (n + 1)) 0 
    
    let rec check lst =
        match lst with
        |[] -> true
        |hd :: tl -> 
            if hd <= 9 then check tl
            else false
    
    let sign a b =
        if (a = -1 && b = -1) || (a = 1 && b = 1)
        then 1
        else -1 
    
    if lst1 = [] || lst2 = []
    then failwith "Incorrect input: the list is empty"
    elif abs (lst1.Head) <> 1 && abs (lst2.Head) <> 1 
    then failwith "Incorrect input: the sign must be -1 or 1"  
    elif check lst1 && check lst2
    then         
        if lst1.Tail = [0] || lst2.Tail = [0]
        then [0]
        else sign lst1.Head lst2.Head :: List.rev (multiplication (List.rev lst1.Tail) (List.rev lst2.Tail) 0)
    else failwith "Incorrect input: elements of list's must be in 0..9"

printfn "(-864968) * 23 = %A" (main [-1; 8; 6; 4; 9; 6; 8] [1; 2; 3])
printfn "66 * (-77) = %A" (main [1; 6; 6] [-1; 7; 7])
printfn "0 * 6 = %A" (main [1; 0] [1; 6])
printfn "%A" (main [] [])


