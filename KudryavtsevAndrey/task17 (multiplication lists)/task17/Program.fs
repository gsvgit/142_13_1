module Multiplication 

let main (list1:List<_>) (list2:List<_>) = 

    let rec correctionCheck lst = 
        match lst with 
        | hd::tl -> hd < 10 && hd > -10 && correctionCheck tl 
        | [] -> true

    let rec deletingZeroes list = 
        match list with 
        | [] -> [0]
        | hd::tl -> 
            if hd = 0 
            then deletingZeroes tl
            else hd::tl 

    if list1.Length < 2 || list2.Length < 2 
    then failwith "Error! Entered incorrect numbers. " 
    elif abs(List.head list1) <> 1 || abs(List.head list2) <> 1
    then failwith "Error! Entered incorrect sign, sign must be valued 1 or -1. "
    elif not (correctionCheck list1.Tail && correctionCheck list2.Tail)
    then failwith "Error! Entered incorrect digits, digits must be valued from 0 to 9. "
    else  
                                                               
        let rec addition lst1 lst2 excess sign1 sign2 =
            match lst1, lst2 with
            |[], [] -> 
                if excess = 0 
                then []
                else [excess]
            |hd1 :: tl1, hd2 :: tl2 ->                
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then sum1 % 10 :: addition tl1 tl2 (sum1 / 10) sign1 sign2 
                else sum1 :: addition tl1 tl2 (sum1 / 10) sign1 sign2                                    
            |hd1 :: tl1, []
            |[], hd1 :: tl1 ->                 
                let sum2 = hd1 + excess 
                if sum2 > 9 
                then sum2 % 10 :: addition tl1 [] (sum2 / 10) sign1 sign2
                else sum2 :: addition tl1 [] (sum2 / 10) sign1 sign2

        let rec multiplication hd list exc discharge =
            if discharge > 0
            then 0 :: multiplication hd list exc (discharge - 1)
            else
                match list with
                |[] ->
                    if exc > 0 
                    then [exc]
                    else []
                |hd1 :: tl1 ->                   
                    let mult = hd * hd1 + exc
                    if mult > 9 
                    then mult % 10 :: multiplication hd tl1 (mult / 10) 0 
                    else mult :: multiplication hd tl1 0 0

        let rec multi list1 list2 disch =
            match list2 with
            |[] -> []
            |hd1 :: tl1 ->
                addition (multiplication hd1 list1 0 disch) (multi list1 tl1 (disch + 1) ) 0 1 1 

        if List.head list1 = List.head list2
        then 1 :: (multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev |> deletingZeroes)
        else -1 :: (multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev |> deletingZeroes)


printfn "res = %A" (main [1; 9; 9; 9] [1; 0])
printfn "res = %A" (main [1; 1; 2; 3] [-1; 1; 1; 1])  
printfn "res = %A" (main [1; 9] [-1; 1])