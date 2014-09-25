namespace Problem

module Main =
    let main (lst: list<int>) =
        let a = Array.zeroCreate lst.Length
        let rec copy lst i =
            match lst with
            | h :: t ->
                a.[i] <- h
                copy t (i + 1)
            | [] -> a
        copy lst 0