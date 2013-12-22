module MAddition
open TypeList
let rev mlst =
    let rec get lst=
        match lst with
        | Lst(chis, Empty) -> chis
        | Lst(chis, lst1) -> get lst1
        | Empty -> 0
    let rec cut mlst1 =
        match mlst1 with 
        | Lst (chis, Empty) -> Empty
        | Lst (chis, lst) -> Lst (chis, cut lst)
        | Empty -> Empty
    let rec revf mlst1 =
        match mlst1 with
        | Empty -> Empty
        | Lst(chis, lst) -> Lst(get mlst1, revf (cut mlst1))
    revf mlst

let length mlst =
    let rec leng mlst1 k =
        match mlst1 with
        | Lst(chis, Empty) -> k+1
        | Lst(chis, lst) -> leng lst k+1
        | Empty -> 0
    leng mlst 0

let head mlst =
    match mlst with
    | Lst(chis, lst) -> chis

let main lst lst2 =
    // Addition
    let rec go l l2 n=
        match l, l2 with
        | Empty, Empty -> 
                if n = 0
                then Empty
                else Lst(n, Empty)
        | Lst(h, t), Empty
        | Empty, Lst(h, t) -> 
            let x = h + n
            if x > 9 || x < -9 
            then Lst(x, go t Empty (x / 10))
            else Lst(x, go t Empty 0) 
        | Lst(h, t), Lst(h2, t2) ->
            let x = h + h2 + n
            if x > 9 || x < -9 
            then Lst(x % 10, go t t2 (x / 10))
            else Lst(x, go t t2 0)
    // Subtraction
    let rec go2 l l2 n=
        match l, l2 with
        | Empty, Empty -> 
                if n = 0
                then Empty
                else Lst(n, Empty)
        | Lst(h, t), Empty
        | Empty, Lst(h, t) -> 
            let x = h - n
            if x > 9 || x < -9
            then Lst(x, go2 t Empty (x / 10))
            elif h = 0 
            then Lst(x + 10, go2 t Empty 1) 
            else Lst(x, go2 t Empty 0)  
        | Lst(h, t), Lst(h2, t2) ->
            let x = h - h2 - n
            if h < h2 
            then
                if x > 9 || x < -9 
                then Lst((x + 10) % 10, go2 t t2 ((x + 1) / 10))
                else Lst(x + 10, go2 t t2 1)
            else
                if x > 9 || x < (-9)
                then Lst(x % 10, go2 t t2 (x / 10))
                else Lst(x, go2 t t2 0)
            
    // Module of each
    let rec poli lst =
        match lst with
        | Empty -> Empty
        | Lst(h, t) ->
           if h < 0 
           then Lst(h * -1, poli t)
           else Lst(h, poli t)
    // The denial of the last item in the list
    let rec otric list =
        match list with
        | Empty -> Empty
        | Lst(h, Empty) -> Lst(h * -1, Empty)
        | Lst(h, t) -> Lst(h, (otric t))
    // Comparisons of the two lists (all elements > 0)      
    let rec sravn lst1 lst2 =
        if length lst1 > length lst2
        then 1
        elif length lst1 < length lst2
        then 2
        else
            match lst1, lst2 with
            | Empty, Empty -> 0
            | Lst(h1, t1), Lst(h2, t2) ->
                if h1 > h2
                then 1
                elif h1 < h2
                then 2
                else sravn t1 t2
            | Lst(h1, t1), Empty -> 1
            | Empty, Lst(h2, t2) -> 2
    // Removing leading zeros
    let rec cut lis =
        match lis with
        | Empty -> Lst(0, Empty)
        | Lst(hd, tl) ->
            if hd = 0
            then cut tl
            else Lst(hd, tl)
    // The creation of additional variables          
    let lstp = poli lst
    let lst2p = poli lst2
    let n = sravn lstp lst2p
    //Negative - Positive 
    if n=1 && head lst < 0 && head lst2 > 0
    then rev (otric (rev (cut (rev (go2 (rev lstp) (rev lst2p) 0)))))
    elif n=2 && head lst < 0 && head lst2 > 0
    then rev (rev (cut (rev (go2 (rev lst2p) (rev lstp) 0))))
    elif n = 0 && head lst < 0 && head lst2 > 0 
    then rev (rev (cut (rev (go2 (rev lstp) (rev lst2p) 0))))
    //Positive - Negative
    elif n=1 && head lst > 0 && head lst2 < 0 
    then rev (rev (cut (rev (go2 (rev lstp) (rev lst2p) 0))))
    elif n=2 && head lst > 0 && head lst2 < 0 
    then rev (otric (rev (cut (rev (go2 (rev lst2p) (rev lstp) 0)))))
    elif n = 0 && head lst > 0 && head lst2 < 0 
    then rev (rev (cut (rev (go2 (rev lstp) (rev lst2p) 0))))
    //Negative - Negative
    elif head lst < 0 && head lst2 < 0 
    then rev (otric (rev (cut (rev (go (rev lstp) (rev lst2p) 0)))))
    //Positive - Positive
    else rev (rev (cut (rev (go (rev lst) (rev lst2) 0))))
//Plate of number given by the first digit. (1 or -1 at the beginning are not indicative of the sign and is the part of the number)
printfn "res2 = %A" (main (Lst(1, Lst(6, Lst(6, Lst(0, Empty))))) (Lst(-1, Lst(6, Lst(6, Lst(0, Empty))))))
printfn "res2 = %A" (main (Lst(-1, Lst(6, Lst(6, Lst(0, Empty))))) (Lst(1, Lst(6, Lst(6, Lst(0, Empty))))))
printfn "res2 = %A" (main (Lst(1, Lst(6, Lst(6, Lst(0, Empty))))) (Lst(-1, Lst(5, Lst(7, Lst(9, Empty))))))
printfn "res2 = %A" (main ( Lst(1, Empty)) (Lst(-1, Lst(6, Lst(6, Lst(0, Empty))))))