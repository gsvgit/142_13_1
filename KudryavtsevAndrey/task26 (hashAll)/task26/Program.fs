open hash 

let hashAll (lst: List<string>) =
    let rec hash list = 
        match list with 
        | [] -> [] 
        | hd::tl -> hashString hd::hash tl
    hash lst

printfn "%A" (hashAll [])
printfn "%A" (hashAll ["1"; "2"; "3"])
printfn "%A" (hashAll ["matmeh"; "lu4she"; "vseh"])