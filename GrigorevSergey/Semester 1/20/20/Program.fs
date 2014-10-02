namespace Problem

module Main =
    let main1 lst arr = // Using .Net library
        let filterFunction x =
            Array.fold (fun state elem -> elem <> x && state) true arr
        lst |> List.filter filterFunction

    let main2 lst arr = // Without usage of .Net library
        let rec exists elem index = 
            if index >= Array.length arr
            then false
            elif elem = arr.[index]
            then true
            else exists elem (index + 1)
        let rec fold lst acc =
            match lst with
            | h :: t ->
                if exists h 0
                then fold t acc
                else List.append acc [h] |> fold t
            | [] -> acc
        fold lst []

    let mainFilter a b = 
        let x = main1 a b
        let y = main2 a b
        if x = y
        then x
        else failwith "One or more implementations work incorrect!"