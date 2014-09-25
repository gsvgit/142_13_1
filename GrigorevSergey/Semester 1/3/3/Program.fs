namespace Problem

module Main =
    let main inArray num =
        inArray
        |> Array.mapi (fun index elem -> if elem > num then index else -1)
        |> Array.filter (fun elem -> if elem < 0 then false else true)