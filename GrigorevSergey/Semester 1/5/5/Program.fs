namespace Problem

open System

module Main =
    let main a =
        if Array.length a = 2
        then
            a.[0] <- a.[0] - a.[1]
            a.[1] <- a.[0] + a.[1]
            a.[0] <- -a.[0] + a.[1]
            a
        else ArgumentException ("Invalid input array length (must be 2)") |> raise