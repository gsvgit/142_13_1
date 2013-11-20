module Multiplication
let main (list1:List< _ >) (list2:List< _ >) = 
    let rec TwoValuedNumber lst =
        match lst with
        |hd::tl -> hd < 10 && TwoValuedNumber tl
        |[] -> true
    if list1.Length < 2 || list2.Length < 2 
    then failwith "Error. The number is incorrectly set"
    elif abs(List.head list1) <> 1 || abs(List.head list2) <> 1
    then failwith "Error. the sign can be set only 1 or -1"
    elif not (TwoValuedNumber list1.Tail && TwoValuedNumber list2.Tail)
    then failwith "Error. The digit can't be two-valued number"
    else                                                             
        let rec addition lst1 lst2 excess sign1 sign2 =
            match lst1, lst2 with
            |[], [] -> 
                if excess = 0 
                then []
                else [excess]
            |hd1::tl1, hd2::tl2 ->                
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then sum1 % 10::addition tl1 tl2 (sum1 / 10) sign1 sign2 
                else sum1::addition tl1 tl2 (sum1 / 10) sign1 sign2                                    
            |hd1::tl1,[]
            |[], hd1::tl1 ->                 
                let sum2 = hd1 * 1 + excess 
                if sum2 > 9 
                then sum2 % 10::addition tl1 [] (sum2 / 10) sign1 sign2
                else sum2::addition tl1 [] (sum2 / 10) sign1 sign2
        let rec multiplication hd list ecx discharge =
            if discharge > 0
            then 0::multiplication hd list ecx (discharge - 1)
            else
                match list with
                |[] ->
                    if ecx > 0 
                    then [ecx]
                    else []
                |hd3::tl3 ->                   
                    let mul = hd * hd3 + ecx
                    if mul > 9 
                    then mul % 10::multiplication hd tl3 (mul / 10) 0 
                    else mul::multiplication hd tl3 0 0
        let rec multi list1 list2 disch =
            match list2 with
            |[]->[]
            |hd4::tl4 ->
                addition (multiplication hd4 list1 0 disch) (multi list1 tl4 (disch + 1) ) 0 1 1 
        if List.head list1 = List.head list1
        then 1::(multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev)
        else -1::(multi (List.rev list1.Tail) (List.rev list2.Tail) 0 |> List.rev)
printfn "1. = %A" (main [1; 9; 9; 9] [1; 8; 8; 8])
printfn "2. = %A" (main [-1; 1; 2; 3] [-1; 1; 2; 3])  
printfn "3. = %A" (main [] [-1; 1; 2; 3])        