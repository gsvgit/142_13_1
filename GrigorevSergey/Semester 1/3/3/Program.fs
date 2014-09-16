namespace Problem3

module Main =
    let main inArray num =
        Array.mapi (fun index elem -> if elem > num then index else -1) inArray
        |> Array.filter (fun elem -> if elem < 0 then false else true)