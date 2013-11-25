let rec filter (lst: int List) (arr: int array) =    
    match lst with
    | [] -> [] 
    | hd :: tl ->
        let check = ref false
        let i = ref 0 
        while (!check = false && !i < arr.Length) do
            if hd = arr.[!i] 
            then check := true
            else i := !i + 1         
        if !check = false 
        then hd :: filter tl arr
        else filter tl arr                   

printfn "%A" (filter [1; 3; 1; 2; 1; 5; 3] [|5; 3|])
printfn "%A" (filter [0] [|1; 8|])
printfn "%A" (filter [] [||])
printfn "%A" (filter [5; 6] [||])
         
        
