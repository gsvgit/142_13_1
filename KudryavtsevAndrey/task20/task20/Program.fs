
let rec mainFilter (lst: int List) (arr: int array) =    
    match lst with
    | [] -> [] 
    | hd :: tl ->
        let a = ref 0
        let i = ref 0 
        while !a = 0 && !i < arr.Length do
            if hd = arr.[!i] 
            then a := 1
            else i := !i + 1         
        if !a = 0
        then hd :: mainFilter tl arr
        else mainFilter tl arr                   

printfn "%A" (mainFilter [1; 2; 3; 4; 5; 6] [|3; 5|])
printfn "%A" (mainFilter [0] [|1|])
printfn "%A" (mainFilter [] [||])
printfn "%A" (mainFilter [1] [||])