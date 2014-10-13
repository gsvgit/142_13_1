namespace Problem

open System

module Main =
    let main (a: array<_>) =
        let r = new Random ()
        let swap i j =
            if i <> j
            then 
                let tmp = a.[i]
                a.[i] <- a.[j]
                a.[j] <- tmp
        let rec sort first last =
            if last - first >= 1
            then
                swap first (first + r.Next (last - first + 1))
                let i = ref (first + 1)
                for j in [first + 1 .. last] do
                    if a.[j] < a.[first]
                    then 
                        swap !i j
                        i := !i + 1
                swap first (!i - 1)
                sort first (!i - 2)
                sort !i last
        sort 0 (a.Length - 1)
        a