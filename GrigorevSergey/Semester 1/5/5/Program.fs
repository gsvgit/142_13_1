﻿namespace Problem

module Main =
    let main (a: array<_>) =
        a.[0] <- a.[0] - a.[1]
        a.[1] <- a.[0] + a.[1]
        a.[0] <- -a.[0] + a.[1]
        a