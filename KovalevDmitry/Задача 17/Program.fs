let main (lst1: int list) (lst2: int list) =    
 
    let rec digitCheck lst =
        match lst with
        | [] -> true
        | hd :: tl -> hd <= 9 && digitCheck tl 
    
    let rec addEndNull lst k =
        if k > 0
        then 0 :: addEndNull lst (k - 1)
        else lst           
    
    let rec multiDigit lst hd m =
        match lst with
        | [] -> 
            if m > 0
            then [m]
            else []
        | hd1 :: tl1 -> 
            let hdMult = hd1 * hd + m
            if hdMult > 9
            then hdMult % 10 :: multiDigit tl1 hd (hdMult / 10) 
            else  hdMult :: multiDigit tl1 hd 0 
                                                            
    let rec sum (lst1: list<int>) (lst2: list<int>) s =          
        match lst1, lst2 with   
        | hd :: tl, [0]  
        | [0], hd :: tl -> hd :: tl
        | [], [] -> 
            if s = 1
            then 1 :: []
            else []                
        | hd1 :: [], hd2 :: [] -> 
            let hdSum = hd1 + hd2 + s 
            if hdSum >= 10 
            then hdSum - 10 :: [1]            
            else hdSum :: [] 
        | hd :: tl, [] ->
            let hs = hd + s
            if hs > 9
            then (hs - 10) :: sum tl [] 1
            else hs :: tl
        | [], hd :: tl ->    
            let hs = hd + s
            if hs > 9
            then (hs - 10) :: sum [] tl 1
            else hs :: tl                             
        | hd1 :: tl1, hd2 :: tl2 -> 
            let hdSum = hd1 + hd2 + s  
            if hdSum >= 10  
            then hdSum - 10 :: sum tl1 tl2 1            
            else hdSum :: sum tl1 tl2 0 
    
    let rec multiList lst1 lst2 n =       
        match lst2 with        
        | [] -> []        
        | hd1 :: tl1 ->  sum (addEndNull (multiDigit lst1 hd1 0) n) (multiList lst1 tl1 (n + 1)) 0 
                                                                     
    let signCheck a b =
        if a = b
        then 1
        else -1
    
    if lst1 = [] || lst2 = []
    then failwith "Empty list"
    elif digitCheck lst1 && digitCheck lst2
    then         
        if lst1.Head = 0 || lst2.Head = 0
        then [0]
        else signCheck lst1.Head lst2.Head :: List.rev (multiList (List.rev lst1.Tail) (List.rev lst2.Tail) 0)                   
    else failwith "Incorrect input list"
          
main [1; 9; 9] [-1; 1; 0; 0] |> printfn "%A"     
main [1; 6; 5; 0] [1; 3; 0] |> printfn "%A"   
main [-1; 8; 2; 3; 0] [-1; 4; 0; 2; 5] |> printfn "%A"
main [1; 4; 2] [0] |> printfn "%A"
main [1; 15; 4] [-1; 6; 7] |> printfn "%A" 