let mainConvert (lst : List<int>) =
    let arr = Array.zeroCreate lst.Length 
    let rec convert (lst1 : List<int>) (arr1 : array<int>) i = 
        match lst1 with
        | hd::tl ->             
            arr.[i] <- hd
            convert tl arr1 (i + 1)            
        | [] -> arr
    convert lst arr 0

printfn "%A" (mainConvert [1; 3; 1; 2; 1; 1; 9])
printfn "%A" (mainConvert [])

