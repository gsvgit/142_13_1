
let mainConvert (list:List<int>)  =
    let arr = Array.zeroCreate (List.length list) 
    let rec convert lst i = 
        match lst with 
        | [] -> arr
        | hd::tl -> 
            arr.[i] <- hd
            convert tl (i + 1)
    convert list 0 

printfn "result = %A" (mainConvert [1; 2; 3; 4; 5])
printfn "result = %A" (mainConvert [0])
printfn "result = %A" (mainConvert [])