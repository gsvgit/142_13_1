module Hash

let stringHash (s: string) = 
    let h = ref 0
    for i in 0..s.Length - 1 do
        
        if s.[i] = '1' 
        || s.[i] = '2' 
        || s.[i] = '3' 
        || s.[i] = '4' 
        || s.[i] = '5' 
        || s.[i] = '6' 
        || s.[i] = '7' 
        || s.[i] = '8' 
        || s.[i] = '9' 
        || s.[i] = '0' 
        then h := !h + 1
    !h
printfn "%A" (stringHash "sfg")
printfn "%A" (stringHash "1fewhh3124")