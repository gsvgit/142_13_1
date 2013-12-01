let mainFilter lst (arr:int[]) =
    let rec del l n =
        match l with
        | [] -> []
        | h::t ->
            if h = n
            then  del t n
            else  h::del t n
    let rec filt l i =
        if i < arr.Length - 1
        then
            filt (del l arr.[i]) (i + 1)
        else del l arr.[i]
    if arr.Length > 0
    then filt lst 0
    else lst
printfn "%A" (mainFilter [1; 8; 2; 2; 3; 5] [|1; 2|])
printfn "%A" (mainFilter [1; 8; 2; 2; 3; 5] [||])