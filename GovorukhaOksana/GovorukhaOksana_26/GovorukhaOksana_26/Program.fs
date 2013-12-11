
open Hash

let rec hashall (lst: List<string>) =
    match lst with
    | hd :: tl -> Hash.stringHash hd :: hashall tl
    | [] -> []

hashall ["Dinner is cooking"; "Go eating "] |> printfn "%A"  
