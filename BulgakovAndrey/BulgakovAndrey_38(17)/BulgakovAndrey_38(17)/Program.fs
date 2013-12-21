type MyList =
    | Lst of int * MyList
    | Empty

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
    
let tail mlst =
    match mlst with
    | Lst(chis, lst) -> lst

let main lst1 lst2 =
    // Addition     
    let rec summ lst1 lst2 s =          
        match lst1, lst2 with   
        | Lst(hd, tl), Lst(0, Empty) -> Lst(hd, tl)
        | Lst(0, Empty), Lst(hd, tl) -> Lst(hd, tl)
        | Empty, Empty -> Empty    
        | Lst(hd1, Empty), Lst(hd2, Empty) -> 
            let hs = hd1 + hd2 + s 
            if hs >= 10 
            then Lst(hs - 10, Lst(1, Empty))            
            else Lst(hs, Empty) 
        | Lst(hd, tl), Empty ->
            let hs = hd + s
            if s = 1 
            then Lst(hs, tl)            
            else Lst(hd, tl)
        | Empty, Lst(hd, tl) ->    
            let hs = hd + s
            if s = 1 
            then Lst(hs, tl)            
            else Lst(hd, tl)                 
        | Lst(hd1, tl1), Lst(hd2, tl2) -> 
            let hs = hd1 + hd2 + s  
            if hs >= 10  
            then Lst(hs - 10, summ tl1 tl2 1)            
            else Lst(hs, summ tl1 tl2 0)
    //Check the numbers in the list         
    let rec prov lst =
        match lst with
        | Empty -> true
        | Lst(hd, tl) -> 
            if hd <= 9 then prov tl
            else false
             
    //Creating zeros
    let rec addzero lst n =
        if n > 0
        then Lst(0, addzero lst (n - 1))
        else lst 
                  
   //Multiplication
    let rec umn lst hd m =
        match lst with
        | Empty -> 
            if m > 0
            then Lst(m, Empty)
            else Empty
        | Lst(hd1, tl1 ) -> 
            let hdMult = hd1 * hd + m
            if hdMult > 9
            then Lst(hdMult % 10, umn tl1 hd (hdMult / 10)) 
            else  Lst(hdMult, umn tl1 hd 0)
             
    //Multiplication
    let rec lstumn lst1 lst2 n =       
        match lst2 with        
        | Empty -> Empty        
        | Lst(hd1, tl1) ->  summ (addzero (umn lst1 hd1 0) n) (lstumn lst1 tl1 (n + 1)) 0 
    
    //The calculation of the mark                                                                 
    let znak a b =
        if a = b
        then 1
        else -1
    
    if lst1 = Empty || lst2 = Empty || lst1 = Lst(1, Empty) || lst2 = Lst(1, Empty) || lst1 = Lst(-1, Empty) || lst2 = Lst(-1, Empty) 
    then failwith "Empty list"
    elif head lst1 <> -1 && head lst2 <> -1 && head lst1 <> 1 && head lst2 <> 1 
    then failwith "Incorrect mark"  
    elif prov lst1 && prov lst2
    then         
        if lst1 = Lst(0, Empty) || lst2 = Lst(0, Empty)
        then Lst(0, Empty)
        else Lst((znak (head lst1) (head lst2)), (rev (lstumn (rev (tail lst1)) (rev (tail lst2)) 0)))
    else failwith "Incorrect input"
printfn "res1 = %A" (main (Lst(1, Lst(6, Lst(6, Lst(0, Empty))))) (Lst(-1, Lst(6, Lst(6, Lst(0, Empty))))))
printfn "res2 = %A" (main (Lst(-1, Lst(6, Lst(6, Lst(0, Empty))))) (Lst(1, Lst(6, Lst(6, Lst(0, Empty))))))
printfn "res3 = %A" (main (Lst(1, Lst(6, Lst(6, Lst(0, Empty))))) (Lst(-1, Lst(5, Lst(7, Lst(9, Empty))))))
printfn "res4 = %A" (main ( Lst(1, Empty)) (Lst(-1, Lst(6, Lst(6, Lst(0, Empty))))))