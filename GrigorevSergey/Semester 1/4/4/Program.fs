namespace Problem

module Main =
    let main inArray lowBound highBound =
        inArray
        |> Array.mapi (fun index elem -> if elem > lowBound && elem < highBound then index else -1)
        |> Array.filter (fun elem -> if elem < 0 then false else true)