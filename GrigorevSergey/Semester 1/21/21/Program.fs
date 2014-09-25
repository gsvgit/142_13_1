namespace Problem

module Main =
    let main (a: array<int>) (b: array<int>) =
        let rec sum k n =
            if k < n
            then b.[k] + sum (k + 1) n
            else 0
        let rec cycle n =
            if n < a.Length
            then
                if a.[n] >= 0 && a.[n] < b.Length
                then sum 0 (a.[n] + 1) + cycle (n + 1)
                else sum 0 b.Length + cycle (n + 1)
            else 0
        cycle 0

    let mainSum arr1 arr2 =
        let res = ref 0
        for i in arr1 do
            let n = 
                if i >=0 && i < Array.length arr2
                then i 
                else arr2.Length - 1
            for j in 0 .. n do
                res := !res + arr2.[j]
        !res