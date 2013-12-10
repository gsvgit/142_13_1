module hash 

let hashString (str: string) = 
    let a = ref 0
    for i in 0..str.Length - 1 do 
        a := !a + int(str.[i])
    if !a = 0
    then 0
    else
        a := !a / str.Length 
        !a 
printfn "%A" (hashString "a")
printfn "%A" (hashString "ab")
printfn "%A" (hashString "az")
printfn "%A" (hashString "")