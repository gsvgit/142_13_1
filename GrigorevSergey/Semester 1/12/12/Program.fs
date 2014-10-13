namespace Problem

open System

module Main =
    let main n =
        if n < 0
        then new ArgumentOutOfRangeException () |> raise
        let arr = Array.zeroCreate (n + 1)
        let rec fib k a b =
            if k = n
            then arr.[n] <- b
            else
                arr.[k] <- b
                fib (k + 1) b (a + b)
        if n <> 0
        then fib 1 0 1
        arr