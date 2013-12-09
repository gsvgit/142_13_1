module Hash

let stringHash str = 
    let hash = ref 0
    for i in 0..String.length str - 1 do
        if i % 2 = 0 
        then hash := !hash + int(str.[i]) 
        else hash := !hash - int(str.[i])
    if !hash < 0 
    then hash := !hash * (-1)
    let y = !hash
    y
    
printfn "res = %A" (stringHash ("abcdefghijklmnopqrstuvwxyz"))
