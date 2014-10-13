namespace Problem

module Main =
    let mainConvert lst =
        let a = List.length lst |> Array.zeroCreate
        let rec copy lst i =
            match lst with
            | h :: t ->
                a.[i] <- h
                copy t (i + 1)
            | [] -> a
        copy lst 0