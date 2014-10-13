namespace Problem

module Main =
    let main n =
        let rec fib a b k =
            if k > 1
            then fib b (a + b) (k - 1)
            else b
        match n with
        | 0 -> 0
        | n when n > 0 -> fib 0 1 n
        | n when n % 2 = 0 -> -fib 0 1 -n
        | n -> fib 0 1 -n