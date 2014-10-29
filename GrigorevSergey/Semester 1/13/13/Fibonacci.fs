namespace Problem

open System

module Fibonacci =
    let fib7 n =
        let rec fib n =
            match n with
            | 0 -> 0
            | 1 -> 1
            | n when n > 1 -> fib (n - 1) + fib (n - 2)
            | n when n % 2 = 0 -> -fib -n
            | n -> fib -n
        fib n

    let fib8 n =
        let a = ref 0
        let b = ref 1
        let m = 
            if n >= 0
            then n
            else -n
        for i in [0 .. m - 2] do
            b := !a + !b
            a := !b - !a
        match n with
        | 0 -> 0
        | n when n > 0 || n % 2 <> 0 -> !b
        | _ -> -(!b)

    let fib9 n =
        let rec fib a b k =
            if k > 1
            then fib b (a + b) (k - 1)
            else b
        match n with
        | 0 -> 0
        | n when n > 0 -> fib 0 1 n
        | n when n % 2 = 0 -> -fib 0 1 -n
        | n -> fib 0 1 -n

    type Matrix<'T> = Matrix of 'T * 'T * 'T * 'T

    let fib10 n =
        let mult a b =
            match a, b with
            | Matrix (a11, a12, a21, a22), Matrix (b11, b12, b21, b22) ->
                let c11 = a11 * b11 + a12 * b21
                let c12 = a11 * b12 + a12 * b22
                let c21 = a21 * b11 + a22 * b21
                let c22 = a21 * b12 + a22 * b22
                Matrix (c11, c12, c21, c22)
        let rec pow n m acc =
             let t = mult acc m
             if n < 1
             then acc
             else pow (n - 1) m t
        let startMatrix = Matrix (1, 1, 1, 0)
        let eMatrix = Matrix (1, 0, 0, 1)
        let res =
            match pow (abs n - 1) startMatrix eMatrix with
            | Matrix (a11, _, _, _) -> a11
        match n with
        | 0 -> 0
        | 1 -> 1
        | n when n % 2 = 0 && n < 0 -> -res
        | _ -> res

    let fib11 n =
        let mult a b =
            match a, b with
            | Matrix (a11, a12, a21, a22), Matrix (b11, b12, b21, b22) ->
                let c11 = a11 * b11 + a12 * b21
                let c12 = a11 * b12 + a12 * b22
                let c21 = a21 * b11 + a22 * b21
                let c22 = a21 * b12 + a22 * b22
                Matrix (c11, c12, c21, c22)
        let rec pow n m acc =
            match n with
            | 0 -> acc
            | n when n % 2 = 0 ->
                let t = mult m m
                pow (n / 2) t acc
            | n ->
                let t = mult m m
                let s = mult acc m
                pow (n / 2) t s
        let startMatrix = Matrix (1, 1, 1, 0)
        let res =
            match pow (abs n - 1) startMatrix (Matrix (1, 0, 0, 1)) with
            | Matrix (a11, _, _, _) -> a11
        match n with
        | 0 -> 0
        | n when n % 2 = 0 && n < 0 -> -res
        | _ -> res

    let fib12 n =
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