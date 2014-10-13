namespace Problem

module Main =
    let main (a: array<_>) =
        let swap i j =
            let tmp = a.[i]
            a.[i] <- a.[j]
            a.[j] <- tmp
        for i in [0 .. a.Length - 2] do
            for j in [0 .. a.Length - 2 - i] do
                if a.[j] > a.[j + 1]
                then swap j (j + 1)
        a