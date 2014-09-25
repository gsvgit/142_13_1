namespace Problem

module Main =
    let main n =
        let rec fib n =
            match n with
            | 0 -> 0
            | 1 -> 1
            | n when n > 1 -> fib (n - 1) + fib (n - 2)
            | n when n % 2 = 0 -> -fib -n
            | n -> fib -n
        fib n