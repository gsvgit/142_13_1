module Addition
let main (lst1: int list) (lst2: int list) =    
    
    let lst1Rev = List.rev (lst1.Tail)
    let lst2Rev = List.rev (lst2.Tail)  
    
    let rec matchLst lst1 lst2 =  
        match lst1, lst2 with
        | [], [] -> false
        | h1 :: t1, h2 :: t2 -> h1 > h2 || matchLst t1 t2
        | _ -> failwith "Incorrect input"                  
    
    let help l1 l2  = 
        List.head l1 = List.head l2 
        || List.head l1 = 1 && List.length l1 > List.length l2
        || List.head l2 = 1 && List.length l2 > List.length l1
        || if List.length l1 = List.length l2 && List.head l1 = 1
           then matchLst (List.tail l1) (List.tail l2)
           elif List.length l1 = List.length l2 && List.head l2 = 1
           then matchLst (List.tail l2) (List.tail l1)
           else false           
    
    let rec deletingZeroes lst =
        match lst with
        | [] -> [0]
        | h :: t ->
             if h = 0
             then deletingZeroes t
             else h :: t   
    
    let rec checkDigit lst =
        match lst with
        | [] -> true
        | hd :: tl -> hd <= 9 && checkDigit tl                                    
    
    let res = help lst1 lst2   
    
    let rec sum lst1 lst2 a b c =  
        match lst1, lst2 with   
        | hd :: tl, [0]  
        | [0], hd :: tl -> hd :: tl
        | [], [] -> 
            if a = 1 
            then 1 :: []  
            else []                        
        | hd1 :: [], hd2 :: [] -> 
            let headSum = hd1 * b + hd2 * c + a 
            if headSum >= 10 
            then headSum - 10 :: [1]
            elif headSum < 0 
            then headSum + 10 :: [1]
            else headSum :: [] 
        | hd1 :: tl1, [] -> 
            let h1 = hd1 * b + a 
            if h1 > 9 
            then (h1 - 10) :: sum tl1 [] 1 b c
            elif h1 < 0 
            then (h1 + 10) :: sum tl1 [] -1 b c
            else h1 :: tl1           
        | [], hd2 :: tl2 -> 
            let h2 = hd2 * c + a 
            if h2 > 9 
            then (h2 - 10) :: sum tl2 [] 1 b c
            elif h2 < 0 
            then (h2 + 10) :: sum tl2 [] -1 b c
            else h2 :: tl2         
        | hd1 :: tl1, hd2 :: tl2 -> 
            let headSum = hd1 * b + hd2 * c + a  
            if headSum >= 10  
            then  headSum - 10 :: sum tl1 tl2 1 b c                                  
            elif headSum < 0 
            then headSum + 10 :: sum tl1 tl2 -1 b c                                 
            else headSum :: sum tl1 tl2 0 b c                 
    
    if checkDigit lst1 && checkDigit lst2
    then
        if res && List.head lst1 = -1 && List.head lst2 = -1
        then -1 :: deletingZeroes (List.rev (sum lst1Rev lst2Rev 0 1 1))
        elif res && List.head lst1 = 1 && List.head lst2 = -1 
             || res && List.head lst1 = -1 && List.head lst2 = 1
        then 1 :: deletingZeroes (List.rev (sum lst1Rev lst2Rev 0 (List.head lst1) (List.head lst2)))               
        elif List.head lst1 = 1 && List.head lst2 = -1  || List.head lst1 = -1 && List.head lst2 = 1            
        then -1 :: deletingZeroes (List.rev (sum lst2Rev lst1Rev 0 (List.head lst1) (List.head lst2)))                 
        else 1 :: deletingZeroes (List.rev (sum lst1Rev lst2Rev 0 1 1))
    else failwith "Error! Digit must be from 0 to 9!"      

main [1; 9; 9; 9; 9; 9; 9] [1; 1] |> printfn "%A"     
main [-1; 1; 2; 3; 4; 5] [1; 5; 4; 3; 2; 1] |> printfn "%A"
main [1; 9; 9] [-1; 1; 0; 0] |> printfn "%A"
main [1; 1; 0] [-1; 5; 9] |> printfn "%A"
main [-1; 10; 4] [1; 2; 3; 4; 5; 6; 7; 8; 9] |> printfn "%A"