let mainConvert (list:List<int>) =
    let arr = Array.zeroCreate list.Length
    let rec convert list1 i = 
        match list1 with
        | hd :: tl ->             
            arr.[i] <- hd
            convert tl (i + 1)            
        | [] -> arr
    convert list 0
printfn "1. %A" (mainConvert [1; 2; 3; 4; 5; 6; 7; 8; 9])
printfn "2. %A" (mainConvert [0])
printfn "3. %A" (mainConvert [])