module Multi
let main lst1 lst2 =
    // Addition     
    let rec summ lst1 lst2 s =          
        match lst1, lst2 with   
        | hd :: tl, [0] -> hd :: tl
        | [0], hd :: tl -> hd :: tl
        | [], [] -> []    
        | hd1 :: [], hd2 :: [] -> 
            let hs = hd1 + hd2 + s 
            if hs >= 10 
            then hs - 10 :: [1]            
            else hs :: [] 
        | hd :: tl, [] ->
            let hs = hd + s
            if s = 1 
            then hs :: tl            
            else hd :: tl
        | [], hd :: tl ->    
            let hs = hd + s
            if s = 1 
            then hs :: tl            
            else hd :: tl                 
        | hd1 :: tl1, hd2 :: tl2 -> 
            let hs = hd1 + hd2 + s  
            if hs >= 10  
            then hs - 10 :: summ tl1 tl2 1            
            else hs :: summ tl1 tl2 0
    //Check the numbers in the list         
    let rec prov lst =
        match lst with
        | [] -> true
        | hd :: tl -> 
            if hd <= 9 then prov tl
            else false
             
    //Creating zeros
    let rec addzero lst n =
        if n > 0
        then 0 :: addzero lst (n - 1)
        else lst 
                  
   //Multiplication
    let rec umn lst hd m =
        match lst with
        | [] -> 
            if m > 0
            then [m]
            else []
        | hd1 :: tl1 -> 
            let hdMult = hd1 * hd + m
            if hdMult > 9
            then hdMult % 10 :: umn tl1 hd (hdMult / 10) 
            else  hdMult :: umn tl1 hd 0
             
    //Multiplication
    let rec lstumn lst1 lst2 n =       
        match lst2 with        
        | [] -> []        
        | hd1 :: tl1 ->  summ (addzero (umn lst1 hd1 0) n) (lstumn lst1 tl1 (n + 1)) 0 
    
    //The calculation of the mark                                                                 
    let znak a b =
        if a = b
        then 1
        else -1
    
    if lst1 = [] || lst2 = []
    then failwith "Empty list"
    elif lst1.Head <> -1 || lst2.Head <> -1 || lst1.Head <> 1 || lst2.Head <> 1 
    then failwith "Incorrect mark"  
    elif prov lst1 && prov lst2
    then         
        if lst1 = [0] || lst2 = [0]
        then [0]
        else znak lst1.Head lst2.Head :: List.rev (lstumn (List.rev lst1.Tail) (List.rev lst2.Tail) 0)
    else failwith "Incorrect input"
          
main [-5] [-5; 5] |> printfn "res = %A"
printfn "res2 = %A" (main [1; 6; 6; 0] [-1; 6; 6; 0])
printfn "res3 = %A" (main [-1; 6; 6; 0] [1; 6; 6; 0])
printfn "res4 = %A" (main [1; 6; 6; 0] [-1; 5; 7; 9])
printfn "res4 = %A" (main [0; 3] [6; 3])
printfn "res5 = %A" (main [1; 6; 6; 0] [-1; 7; 5; 9])
printfn "res6 = %A" (main [1] [-1; 6; 6; 0])
printfn "res7 = %A" (main [-1; 6; 6; 0] [1])
printfn "res8 = %A" (main [-1; 9; 9; 9] [-1; 8; 8; 8])