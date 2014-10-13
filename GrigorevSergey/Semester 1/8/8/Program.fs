namespace Problem

module Main = 
    let main n =
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