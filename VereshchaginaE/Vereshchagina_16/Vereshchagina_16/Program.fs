module Addition
let main lst lst2 =
    
    let rec addition l l2 n=
        match l, l2 with
        | hd :: tl, hd2 :: tl2 ->
            let x = hd + hd2 + n
            if abs(x) > 9 
            then x % 10 :: addition tl tl2 (x / 10)
            else x :: addition tl tl2 0
        | hd :: tl, []
        | [], hd :: tl -> 
            let x = hd + n
            if abs(x) > 9 
            then x :: addition tl [] (x / 10)
            else x :: addition tl [] 0
        | [], [] -> 
                if n = 0
                then []
                else [n]
         
    let rec subtraction l l2 n=
        match l, l2 with
        | hd :: tl, hd2 :: tl2 ->
            let x = hd - hd2 - n
            if hd < hd2 
            then
                if abs(x) > 9 
                then (x + 10) % 10 :: subtraction tl tl2 ((x + 1) / 10)
                else x + 10 :: subtraction tl tl2 1
            else
                if abs(x) > 9
                then x % 10 :: subtraction tl tl2 (x / 10)
                else x :: subtraction tl tl2 0
        | hd :: tl, []
        | [], hd :: tl -> 
            let x = hd - n
            if abs(x) > 9
            then x :: subtraction tl [] (x / 10)
            elif hd = 0 
            then x + 10 :: subtraction tl [] 1 
            else x :: subtraction tl [] 0 
        | [], [] -> 
                if n = 0
                then []
                else [n]    
        
    let rec delzeros lis =
        match lis with
        | [] -> [0]
        | hd :: tl ->
            if hd = 0
            then delzeros tl
            else hd :: tl

    let rec abst lst =
        match lst with
        | h :: t ->
           if h < 0 
           then h * -1 :: abst t
           else h :: abst t
        | [] -> []
    
    let rec denial list =
        match list with
        | h :: [] -> h * -1 :: []
        | h :: t -> h :: (denial t)
        | [] -> []
          
    let rec comparison lst1 lst2 =
        if List.length lst1 > List.length lst2
        then 1
        elif List.length lst1 < List.length lst2
        then 2
        else
            match lst1, lst2 with
            | h1 :: t1, h2 :: t2 ->
                if h1 > h2
                then 1
                elif h1 < h2
                then 2
                else comparison t1 t2
            | h1 :: t1, [] -> 1
            | [], h2 :: t2 -> 2
            | [], [] -> 0
    
    let lstp = abst lst
    let lst2p = abst lst2
    let n = comparison lstp lst2p
     
    if n=1 && List.head lst < 0 && List.head lst2 > 0
    then List.rev (denial (List.rev (delzeros (List.rev (subtraction (List.rev lstp) (List.rev lst2p) 0)))))
    elif n=2 && List.head lst < 0 && List.head lst2 > 0
    then List.rev (List.rev (delzeros (List.rev (subtraction (List.rev lst2p) (List.rev lstp) 0))))
    elif n = 0 && List.head lst < 0 && List.head lst2 > 0 
    then List.rev (List.rev (delzeros (List.rev (subtraction (List.rev lstp) (List.rev lst2p) 0))))
    elif n=1 && List.head lst > 0 && List.head lst2 < 0 
    then List.rev (List.rev (delzeros (List.rev (subtraction (List.rev lstp) (List.rev lst2p) 0))))
    elif n=2 && List.head lst > 0 && List.head lst2 < 0 
    then List.rev (denial (List.rev (delzeros (List.rev (subtraction (List.rev lst2p) (List.rev lstp) 0)))))
    elif n = 0 && List.head lst > 0 && List.head lst2 < 0 
    then List.rev (List.rev (delzeros (List.rev (subtraction (List.rev lstp) (List.rev lst2p) 0))))
    elif List.head lst < 0 && List.head lst2 < 0 
    then List.rev (denial (List.rev (delzeros (List.rev (addition (List.rev lstp) (List.rev lst2p) 0)))))
    else List.rev (List.rev (delzeros (List.rev (addition (List.rev lst) (List.rev lst2) 0))))

printfn "-86 - 10 = %A" (main [-8; 6] [-1; 0])
printfn "-1 + 1 = %A" (main [-1] [1])
printfn "66 + 77 = %A" (main [6; 6] [7; 7])
printfn "0 + 0 = %A" (main [0] [0])

