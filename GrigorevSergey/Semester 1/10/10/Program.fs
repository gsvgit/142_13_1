namespace Problem

open System

module Main =
    type Matrix<'T> = Matrix of 'T * 'T * 'T * 'T

    let fib n =
        let mult a b =
            match a, b with
            | Matrix (a11, a12, a21, a22), Matrix (b11, b12, b21, b22) ->
                let c11 = a11 * b11 + a12 * b21
                let c12 = a11 * b12 + a12 * b22
                let c21 = a21 * b11 + a22 * b21
                let c22 = a21 * b12 + a22 * b22
                Matrix (c11, c12, c21, c22)
        let rec pow n m =
            if n > 1
            then mult m (pow (n - 1) m)
            else m
        let startMatrix = Matrix (1, 1, 1, 0)
        let res =
            match pow (abs n - 1) startMatrix with
            | Matrix (a11, _, _, _) -> a11
        match n with
        | 0 -> 0
        | n when n % 2 = 0 && n < 0 -> -res
        | _ -> res