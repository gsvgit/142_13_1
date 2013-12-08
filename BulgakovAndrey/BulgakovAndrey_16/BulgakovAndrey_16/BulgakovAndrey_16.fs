module Addition
let main lst lst2 =
    // Addition
    let rec go l l2 n=
        match l, l2 with
        | [], [] -> 
                if n = 0
                then []
                else [n]
        | h :: t, []
        | [], h :: t -> 
            let x = h + n
            if x > 9 || x < -9 
            then x :: go t [] (x / 10)
            else x :: go t [] 0 
        | h :: t, h2 :: t2 ->
            let x = h + h2 + n
            if x > 9 || x < -9 
            then x % 10 :: go t t2 (x / 10)
            else x :: go t t2 0
    // Subtraction
    let rec go2 l l2 n=
        match l, l2 with
        | [], [] -> 
                if n = 0
                then []
                else [n]
        | h :: t, []
        | [], h :: t -> 
            let x = h - n
            if x > 9 || x < -9
            then x :: go2 t [] (x / 10)
            elif h = 0 
            then x + 10 :: go2 t [] 1 
            else x :: go2 t [] 0  
        | h :: t, h2 :: t2 ->
            let x = h - h2 - n
            if h < h2 
            then
                if x > 9 || x < -9 
                then (x + 10) % 10 :: go2 t t2 ((x + 1) / 10)
                else x + 10 :: go2 t t2 1
            else
                if x > 9 || x < (-9)
                then x % 10 :: go2 t t2 (x / 10)
                else x :: go2 t t2 0
            
    // Module of each
    let rec poli lst =
        match lst with
        | [] -> []
        | h :: t ->
           if h < 0 
           then h * -1 :: poli t
           else h :: poli t
    // The denial of the last item in the list
    let rec otric list =
        match list with
        | [] -> []
        | h :: [] -> h * -1 :: []
        | h :: t -> h :: (otric t)
    // Comparisons of the two lists (all elements > 0)      
    let rec sravn lst1 lst2 =
        if List.length lst1 > List.length lst2
        then 1
        elif List.length lst1 < List.length lst2
        then 2
        else
            match lst1, lst2 with
            | [], [] -> 0
            | h1 :: t1, h2 :: t2 ->
                if h1 > h2
                then 1
                elif h1 < h2
                then 2
                else sravn t1 t2
            | h1 :: t1, [] -> 1
            | [], h2 :: t2 -> 2
    // Removing leading zeros
    let rec cut lis =
        match lis with
        | [] -> [0]
        | hd :: tl ->
            if hd = 0
            then cut tl
            else hd :: tl
    // The creation of additional variables          
    let lstp = poli lst
    let lst2p = poli lst2
    let n = sravn lstp lst2p
    //Negative - Positive 
    if n=1 && List.head lst < 0 && List.head lst2 > 0
    then List.rev (otric (List.rev (cut (List.rev (go2 (List.rev lstp) (List.rev lst2p) 0)))))
    elif n=2 && List.head lst < 0 && List.head lst2 > 0
    then List.rev (List.rev (cut (List.rev (go2 (List.rev lst2p) (List.rev lstp) 0))))
    elif n = 0 && List.head lst < 0 && List.head lst2 > 0 
    then List.rev (List.rev (cut (List.rev (go2 (List.rev lstp) (List.rev lst2p) 0))))
    //Positive - Negative
    elif n=1 && List.head lst > 0 && List.head lst2 < 0 
    then List.rev (List.rev (cut (List.rev (go2 (List.rev lstp) (List.rev lst2p) 0))))
    elif n=2 && List.head lst > 0 && List.head lst2 < 0 
    then List.rev (otric (List.rev (cut (List.rev (go2 (List.rev lst2p) (List.rev lstp) 0)))))
    elif n = 0 && List.head lst > 0 && List.head lst2 < 0 
    then List.rev (List.rev (cut (List.rev (go2 (List.rev lstp) (List.rev lst2p) 0))))
    //Negative - Negative
    elif List.head lst < 0 && List.head lst2 < 0 
    then List.rev (otric (List.rev (cut (List.rev (go (List.rev lstp) (List.rev lst2p) 0)))))
    //Positive - Positive
    else List.rev (List.rev (cut (List.rev (go (List.rev lst) (List.rev lst2) 0))))
//Plate of number given by the first digit. (1 or -1 at the beginning are not indicative of the sign and is the part of the number)
main [-5] [-5; 5] |> printfn "res = %A"
printfn "res2 = %A" (main [1; 6; 6; 0] [-1; 6; 6; 0])
printfn "res3 = %A" (main [-1; 6; 6; 0] [1; 6; 6; 0])
printfn "res4 = %A" (main [1; 6; 6; 0] [-1; 5; 7; 9])
printfn "res4 = %A" (main [6; 3] [6; 3])
printfn "res5 = %A" (main [1; 6; 6; 0] [-1; 7; 5; 9])
printfn "res6 = %A" (main [1] [-1; 6; 6; 0])
printfn "res7 = %A" (main [-1; 6; 6; 0] [1])
printfn "res8 = %A" (main [-1; 9; 9; 9] [-1; 8; 8; 8])