open Hash25
let rec hashAll list =
    match list with 
    | hd :: tl -> strHash hd :: hashAll tl
    | [] -> []
    
printfn "%A" (hashAll ["abcdefg"; "5r"; "john16"; ""])
printfn "%A" (hashAll [])