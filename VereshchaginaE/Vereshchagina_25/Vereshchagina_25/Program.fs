let stringHash str = 
    let hash = ref 0
    for i in 0..String.length str - 1 do
        if i % 2 = 0 
        then hash := !hash + int(str.[i]) 
        else hash := !hash - int(str.[i])
    if !hash < 0 
    then hash := !hash * (-1)
    hash

let hashAll (list : List<string>) =
    let rec hash lst =
        match lst with
        | [] -> []
        | head::tail -> stringHash head::hash tail
    hash list    

printfn "%A" (hashAll [])
printfn "%A" (hashAll ["1"; "2"; "3"])
printfn "%A" (hashAll ["Hello";"World"])


    


