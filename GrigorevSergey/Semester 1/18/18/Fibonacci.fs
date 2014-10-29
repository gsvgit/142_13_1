namespace Problem

    open System

    module Fibonacci = 
        open BigIntegers   
        
        let fib7 n =
            let rec fib n =
                match n with
                | BigInteger (Zero, _) -> BigInteger.Zero
                | BigInteger (Plus, [1]) -> BigInteger.One
                | n when n > BigInteger.One -> fib (n - BigInteger.One) + fib (n - new BigInteger (2))
                | n when n.IsEven -> -fib -n
                | n -> fib -n
            fib n

        let fib8 n =
            let a = ref (BigInteger.Zero)
            let b = ref (BigInteger.One)
            let m = 
                if n >= BigInteger.Zero
                then n
                else -n
            for i in BigInteger.Zero .. m - new BigInteger (2) do
                b := !a + !b
                a := !b - !a
            match n with
            | BigInteger (Zero, _) -> BigInteger.Zero
            | n when n > BigInteger.Zero || n.IsOdd -> !b
            | _ -> -(!b)

        let fib9 n =
            let rec fib a b k =
                if k > BigInteger.One
                then fib b (a + b) (k - BigInteger.One)
                else b
            match n with
            | BigInteger (Zero, _) -> BigInteger.Zero
            | n when n > BigInteger.Zero -> fib BigInteger.Zero BigInteger.One n
            | n when n.IsEven -> -fib BigInteger.Zero BigInteger.One -n
            | n -> fib BigInteger.Zero BigInteger.One -n

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
            let rec pow n m =
                if n > BigInteger.One
                then mult m (pow (n - BigInteger.One) m)
                else m
            let startMatrix = Matrix (BigInteger.One, BigInteger.One, BigInteger.One, BigInteger.Zero)
            let res =
                match pow (abs n - BigInteger.One) startMatrix with
                | Matrix (a11, _, _, _) -> a11
            match n with
            | BigInteger (Zero, _) -> BigInteger.Zero
            | n when n.IsEven && n < BigInteger.Zero -> -res
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
            let rec pow n m =
                match n with
                | n when n <= BigInteger.One -> m
                | n when n.IsEven -> pow n.Half (mult m m)
                | n -> mult m (pow n.Half (mult m m))
            let startMatrix = Matrix (BigInteger.One, BigInteger.One, BigInteger.One, BigInteger.Zero)
            let res =
                match pow (abs n - BigInteger.One) startMatrix with
                | Matrix (a11, _, _, _) -> a11
            match n with
            | BigInteger (Zero, _) -> BigInteger.Zero
            | n when n.IsEven && n < BigInteger.Zero -> -res
            | _ -> res

        let fib12 n =
            if n < BigInteger.Zero
            then new ArgumentOutOfRangeException () |> raise
            let arr = Array.zeroCreate (n.ToInt32 () + 1)
            arr.[0] <- BigInteger.Zero
            let rec fib k a b =
                if k = n
                then arr.[n.ToInt32 ()] <- b
                else
                    arr.[k.ToInt32 ()] <- b
                    fib (k + BigInteger.One) b (a + b)
            if n <> BigInteger.Zero
            then fib BigInteger.One BigInteger.Zero BigInteger.One
            arr

    module Main =
        open BigIntegers
        open Fibonacci

        [<EntryPoint>]
        let main args =
            let a = [BigInteger.Zero .. new BigInteger (12)]
            let b = new BigInteger (5)
            a |> printfn "%A"
            fib12 b |> printfn "%A"
            0