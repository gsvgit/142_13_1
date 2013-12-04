open hash
let rec hashAll x = 
    match x with
    | [] -> []
    | h::t -> stringHash h::hashAll t
printfn "%A" (hashAll ["London is"; " the"; " capital of G"; "reat Britain"])
printfn "%A" (hashAll ["Kansas"; "Dust in"; " "; "the       wind"])