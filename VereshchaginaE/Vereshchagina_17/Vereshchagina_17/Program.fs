module Multiplication

let main (list1 : List<_>) (list2 : List<_>) = 
    let head1 = List.head list1
    let head2 = List.head list2
    let rec delzero list =
        match list with
        |[] -> [0]
        |hd :: tl ->
            if hd = 0
            then delzero tl
            else hd :: tl  
    
    let rec check lst =
        match lst with
        |hd :: tl -> hd < 10 && check tl
        |[] -> true
    
    if list1.Length < 2 || list2.Length < 2 
    then failwith "Incorrect number"
    elif abs (List.head list1) <> 1 || abs (List.head list2) <> 1
    then failwith "Incorrect sign (can be only 1 or -1)"
    elif not (check list1.Tail && check list2.Tail)
    then failwith "Error. The digit must be in 1..9"
    else                                                             
        let rec addition lst1 lst2 oddment sign1 sign2 =
            match lst1, lst2 with
            |hd1 :: tl1, hd2 :: tl2 ->                
                let sum1 = hd1 * sign1 + hd2 * sign2 + oddment 
                if sum1 > 9 
                then sum1 % 10 :: addition tl1 tl2 (sum1 / 10) sign1 sign2 
                else sum1 :: addition tl1 tl2 (sum1 / 10) sign1 sign2                                    
            |hd1 :: tl1, []
            |[], hd1 :: tl1 ->                 
                let sum2 = hd1 * 1 + oddment 
                if sum2 > 9 
                then sum2 % 10 :: addition tl1 [] (sum2 / 10) sign1 sign2
                else sum2 :: addition tl1 [] (sum2 / 10) sign1 sign2
            |[], [] -> 
                if oddment = 0 
                then []
                else [oddment]
                    
        let rec multiplication hd list oddmt discharge =
            if discharge > 0
            then 0 :: multiplication hd list oddmt (discharge - 1)
            else
                match list with
                |[] ->
                    if oddmt > 0 
                    then [oddmt]
                    else []
                |hd3 :: tl3 ->                   
                    let mul = hd * hd3 + oddmt
                    if mul > 9 
                    then mul % 10 :: multiplication hd tl3 (mul / 10) 0 
                    else mul :: multiplication hd tl3 0 0
        
        let rec multi list1 list2 disch =
            match list2 with
            |[] -> []
            |hd :: tl ->
                addition (multiplication hd list1 0 disch) (multi list1 tl (disch + 1) ) 0 1 1 
        
        if head1 = head2
        then 1 :: (multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev |> delzero)
        else -1 :: (multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev |> delzero)

printfn "(-864968) * 23 = %A" (main [-1; 8; 6; 4; 9; 6; 8] [1; 2; 3])
printfn "66 * (-77) = %A" (main [1; 6; 6] [-1; 7; 7])
printfn "-8 * -6 = %A" (main [-1; 8] [-1; 6])
printfn "8 * 6 = %A" (main [1; 8] [1; 6])
printfn "0 * 6 = %A" (main [1; 0] [1; 6])





