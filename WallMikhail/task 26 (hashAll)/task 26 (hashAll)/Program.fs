open Hash

let rec hashAll lst =
    match lst with 
    | hd :: tl -> stringHash hd :: hashAll tl
    | [] -> []

printfn "%A" (hashAll ["asf124"; "In"; "1111"; "asf3s2"])