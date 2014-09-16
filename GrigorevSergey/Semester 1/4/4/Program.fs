namespace Problem4

module Main =
    let main inArray lowBound highBound =
        Array.mapi (fun index elem -> if elem > lowBound && elem < highBound then index else -1) inArray
        |> Array.filter (fun elem -> if elem < 0 then false else true)