module Qsort
let main (arr : int[]) =
    if arr.Length < 1
    then arr
    else
        let rec qsort l r =
            let x = arr.[(l + r) / 2]
            let i = ref l
            let j = ref r
            while !i <= !j do
                while arr.[!i] < x do i := !i + 1
                while arr.[!j] > x do j := !j - 1
                if !i <= !j
                then 
                    let t = arr.[!i]
                    arr.[!i] <- arr.[!j]
                    arr.[!j] <- t
                    i := !i + 1 
                    j := !j - 1
            if l < !j
            then qsort l !j
            if r > !i
            then qsort !i r
        
        qsort 0 (arr.Length - 1)
        arr
printfn "res = %A" (main [|1; 5; 2; 5; 9; 0; 4|])
printfn "res = %A" (main [|1; 2; 3|])
printfn "res = %A" (main [||])
