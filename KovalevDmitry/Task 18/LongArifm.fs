module LongArifm

let summa (lst1: int list) (lst2: int list) =    
    let l1Rev = List.rev <| lst1.Tail
    let l2Rev = List.rev <| lst2.Tail
    let l1Head = lst1.Head
    let l2Head = lst2.Head
   
    let rec matchLst lst1 lst2 =  
        match lst1, lst2 with
        | [], [] -> false
        | hd1::tl1, hd2::tl2 ->  hd1 > hd2 || matchLst tl1 tl2
        | _ -> failwith "Incorrect input"     
                  
    let modulCheck (l1: int list) (l2: int list)  = 
        l1.Head = l2.Head 
        || l1.Head = 1 && l1.Length > l2.Length
        || l2.Head = 1 && l2.Length > l1.Length
        ||  if l1.Length = l2.Length && l1.Head = 1
            then matchLst l1.Tail l2.Tail
            elif l1.Length = l2.Length && l2.Head = 1
            then matchLst l2.Tail l1.Tail
            else false
           
    let rec nullCut lst =
        match lst with
        | [] -> [0]
        | hd :: tl ->
             if hd = 0
             then nullCut tl
             else hd :: tl
    
    let rec digitCheck lst =
        match lst with
        | [] -> true
        | hd :: tl -> hd <= 9 && digitCheck tl 
                                   
    let posModulCheck = modulCheck lst1 lst2
   
    let rec sum (lst1: list<int>) (lst2: list<int>) s z x =  
        match lst1, lst2 with   
        | hd :: tl, [0]  
        | [0], hd :: tl -> hd :: tl
        | [], [] -> 
            if s = 1 
            then 1 :: []  
            else []                        
        | hd1 :: [], hd2 :: [] -> 
            let hdSum = hd1 * z + hd2 * x + s 
            if hdSum >= 10 
            then hdSum - 10 :: [1]
            elif hdSum < 0 
            then hdSum + 10 :: [1]
            else hdSum :: [] 
        | hd1 :: tl1, [] -> 
            let h1s = hd1 * z + s 
            if h1s > 9 
            then (h1s - 10) :: sum tl1 [] 1 z x
            elif h1s < 0 
            then (h1s + 10) :: sum tl1 [] -1 z x
            else h1s :: tl1           
        | [], hd2 :: tl2 -> 
            let h2s = hd2 * x + s 
            if h2s > 9 
            then (h2s - 10) :: sum tl2 [] 1 z x
            elif h2s < 0 
            then (h2s + 10) :: sum tl2 [] -1 z x
            else h2s :: tl2         
        | hd1 :: tl1, hd2 :: tl2 -> 
            let hdSum = hd1 * z + hd2 * x + s  
            if hdSum >= 10  
            then  hdSum - 10 :: sum tl1 tl2 1 z x                                  
            elif hdSum < 0 
            then hdSum + 10 :: sum tl1 tl2 -1 z x                                 
            else hdSum :: sum tl1 tl2 0 z x                         
                 
    
    if digitCheck lst1 && digitCheck lst2
    then
        if posModulCheck && l1Head = -1 && l2Head = -1
        then -1 :: (sum l1Rev l2Rev 0 1 1 |> List.rev |> nullCut)
        elif posModulCheck && l1Head = 1 && l2Head = -1 
             || posModulCheck && l1Head = -1 && l2Head = 1
        then 1 :: (sum l1Rev l2Rev 0 l1Head l2Head |> List.rev |> nullCut)               
        elif l1Head = 1 && l2Head = -1  || l1Head = -1 && l2Head = 1            
        then -1 :: (sum l1Rev l2Rev 0 l2Head l1Head |> List.rev |> nullCut)                 
        else  1 :: (sum l1Rev l2Rev 0 1 1 |> List.rev |> nullCut )
    else failwith "Incorrect digit in list (digit > 9)"  

let multList (lst1: int list) (lst2: int list) =    
 
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