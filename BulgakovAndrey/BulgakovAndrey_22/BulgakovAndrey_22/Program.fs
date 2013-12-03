let mainConvert list =
    let del lst1 =
        match lst1 with
        | [] -> []
        | h :: t -> t
    let rec f lst (arr:int[]) i =
        if i = arr.Length 
        then arr
        else
            arr.[i] <- List.head lst
            f (del lst) arr (i + 1)
    let array = Array.zeroCreate (List.length list)
    f list array 0
printfn "%A" (mainConvert [4; 7; 3])