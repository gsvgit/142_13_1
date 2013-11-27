let mainFilter lst (arr:int array) =

    let rec del l x =
        match l with
        | [] -> []
        | h :: t ->
            if h = x
            then  del t x
            else  h :: (del t x)

    let rec fil l i =
        if i < arr.Length - 1
        then
            fil (del l arr.[i]) (i + 1)
        else del l arr.[i]
    fil lst 0

printfn "%A" (mainFilter [1; 8; 2; 2; 3; 5] [|1;2|])    