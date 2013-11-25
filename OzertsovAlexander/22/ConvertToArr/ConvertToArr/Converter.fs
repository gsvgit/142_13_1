let main list = 
    let array = Array.zeroCreate (List.length list)
    let rec con list (arr:array<_>) i =
        match list with
        | hd :: tl -> 
            arr.[i] <- hd
            con tl arr (i + 1)
        | [] -> array
    
    con list array 0
printfn "%A" (main [1; 3; 1; 0; 4; 5])
printfn "%A" (main [])