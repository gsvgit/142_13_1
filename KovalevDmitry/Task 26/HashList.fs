open Hash

let rec hashAll (lst: string List) =
    match lst with
    | hd :: tl -> stringHash hd :: hashAll tl
    | [] -> []

hashAll ["mrmrm"; "42"; "mrmrm"; "HELLO_WORLD"] |> printfn "%A"