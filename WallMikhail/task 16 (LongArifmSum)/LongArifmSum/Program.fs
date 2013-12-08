module Addit

let main (lst1: int list) (lst2: int list) =    
    let l1Rev = List.rev <| lst1.Tail
    let l2Rev = List.rev <| lst2.Tail
    let l1Head = lst1.Head
    let l2Head = lst2.Head
   
    let rec matchLst lst1 lst2 =  
        match lst1, lst2 with
        | [], [] -> false
        | hd1 :: tl1, hd2 :: tl2 ->  hd1 > hd2 || matchLst tl1 tl2
        | _ -> failwith "wrong input"     
                  
    let checkSign (l1: int list) (l2: int list)  = 
        l1.Head = l2.Head 
        || l1.Head = 1 && l1.Length > l2.Length
        || l2.Head = 1 && l2.Length > l1.Length
        ||  if l1.Length = l2.Length && l1.Head = 1
            then matchLst l1.Tail l2.Tail
            elif l1.Length = l2.Length && l2.Head = 1
            then matchLst l2.Tail l1.Tail
            else false
           
    let rec cutNull lst =
        match lst with
        | [] -> [0]
        | hd :: tl ->
             if hd = 0
             then cutNull tl
             else hd :: tl
    
    let rec checkDigit lst =
        match lst with
        | [] -> true
        | hd :: tl -> hd <= 9 && checkDigit tl 
                                   
    let checkPosSign = checkSign lst1 lst2
   
    let rec sum lst1 lst2 z x c =  
        match lst1, lst2 with   
        | hd :: tl, [0]  
        | [0], hd :: tl -> hd :: tl
        | [], [] -> 
            if z = 1 
            then 1 :: []  
            else []                        
        | hd1 :: tl1, [] -> 
            let h1s = hd1 * z + z 
            if h1s > 9 
            then (h1s - 10) :: sum tl1 [] 1 x c
            elif h1s < 0 
            then (h1s + 10) :: sum tl1 [] -1 x c
            else h1s :: tl1           
        | [], hd2 :: tl2 -> 
            let h2s = hd2 * x + z
            if h2s > 9 
            then (h2s - 10) :: sum tl2 [] 1 x c
            elif h2s < 0 
            then (h2s + 10) :: sum tl2 [] -1 x c
            else h2s :: tl2         
        | hd1 :: tl1, hd2 :: tl2 -> 
            let hdSum = hd1 * x + hd2 * c + z  
            if hdSum >= 10  
            then  hdSum - 10 :: sum tl1 tl2 1 x c                                 
            elif hdSum < 0 
            then hdSum + 10 :: sum tl1 tl2 -1 x c                                
            else hdSum :: sum tl1 tl2 0 x c    
            
    let revLst list = 
        list |> List.rev |> cutNull 
            
    if checkDigit lst1 && checkDigit lst2
    then
        if checkPosSign && l1Head = -1 && l2Head = -1
        then -1 :: (sum l1Rev l2Rev 0 1 1 |> List.rev |> cutNull)
        elif checkPosSign && l1Head = 1 && l2Head = -1 
             || checkPosSign && l1Head = -1 && l2Head = 1
        then 1 :: (sum l1Rev l2Rev 0 l1Head l2Head |> revLst)               
        elif l1Head = 1 && l2Head = -1  || l1Head = -1 && l2Head = 1            
        then -1 :: (sum l1Rev l2Rev 0 l2Head l1Head |> revLst)                 
        else  1 :: (sum l1Rev l2Rev 0 1 1 |> revLst)
    else failwith "Digit can't be more than 9)"    
    
main [1; 9; 9; 5] [-1; 5; 0] |> printfn "%A"     
main [-1; 6; 6; 6; 1; 5] [1; 7; 7; 0;] |> printfn "%A"
main [-1; 6; 5; 9; 9] [1; 3; 4; 0; 2] |> printfn "%A"   
main [-1; 8; 2; 3; 0] [-1; 4; 0; 2; 5] |> printfn "%A"
main [1; 7; 7; 7] [-1; 7; 7; 7] |> printfn "%A"
main [-1; 1; 2] [1; 25] |> printfn "%A"  
