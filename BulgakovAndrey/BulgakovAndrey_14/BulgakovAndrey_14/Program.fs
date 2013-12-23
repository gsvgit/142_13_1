module Bubble
let main (arr : int[]) =      
    for i in 0..arr.Length - 1 do
        for j in 0..arr.Length - 2 - i do
            if arr.[j] > arr.[j + 1]
            then
                arr.[j] <- arr.[j] + arr.[j + 1]
                arr.[j + 1] <- arr.[j] - arr.[j + 1]
                arr.[j] <- arr.[j] - arr.[j + 1]
    arr
printfn "res = %A" (main [|8; 7; 9; 1; 1|])
printfn "res = %A" (main [||])    
            