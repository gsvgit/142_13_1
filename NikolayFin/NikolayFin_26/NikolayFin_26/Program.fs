open StringHash

let rec hashAll (list:list<string>) = 
    match list with
    | hd :: tl -> stringHash hd :: hashAll tl
    | [] -> []

printfn "%A" (hashAll ["a"; "b"; "c"; "abc"; "cab"; "bada"; ""])