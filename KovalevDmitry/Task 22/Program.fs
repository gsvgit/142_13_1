let mainConvert (lst: int List) =
    
    let rec checkListSize lst =
        match lst with
        | [] -> 0
        | hd :: tl -> checkListSize tl + 1
        
    let arr = Array.create (checkListSize lst) 0 
   
    let rec convert lst (arr: int array) i =
        match lst with 
        | [] -> arr
        | hd :: tl -> 
            arr.[i] <- hd
            convert tl arr (i + 1)
    
    convert lst arr 0

printfn "%A" (mainConvert [5; 5; 5]) 
printfn "%A" (mainConvert [42]) 
printfn "%A" (mainConvert []) 

