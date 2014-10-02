namespace Problem

open System

module Main =
    let main (a: array<_>) i j =
        a.[i] <- a.[i] - a.[j]
        a.[j] <- a.[i] + a.[j]
        a.[i] <- -a.[i] + a.[j]
        a