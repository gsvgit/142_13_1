open Hash

let rec hashAll (list : List<string>) =
    match list with
        | [] -> []
        | head::tail -> Hash.stringHash head::hashAll tail
        

printfn "%A" (hashAll [])
printfn "%A" (hashAll ["1"; "2"; "3"])
printfn "%A" (hashAll ["Hello";"World"])

