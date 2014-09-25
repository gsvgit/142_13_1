namespace Problem

module Main =
    let main (l: list<int>) =
        let a = Array.zeroCreate l.Length
        let rec copy l i =
            match l with
            | h :: t ->
                a.[i] <- h
                copy t (i + 1)
            | [] -> a
        copy l 0