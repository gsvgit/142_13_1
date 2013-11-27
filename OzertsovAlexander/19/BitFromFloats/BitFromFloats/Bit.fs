let main (flt:float) =
    let rec convert (array:array<_>) count =
        if count > 0
        then 
            printf "  %A" array.[count]
            convert array (count - 1)
        else printfn "  %A" array.[count] 
    
    let con = [|0 .. 7|]
    let arr = System.BitConverter.GetBytes(flt)
    
    
    
    for i in 0 .. arr.Length - 1 do
        for j in 0 .. 7 do
            if arr.[i] >= 128uy
            then con.[j] <- 1
            else con.[j] <- 0
            arr.[i] <- arr.[i] <<< 1
        convert con 7
main (1.2345)
System.BitConverter.GetBytes (1.2345) |> printfn"%A"