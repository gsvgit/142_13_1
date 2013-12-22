let mainConvert (lst: list<int>) = 
    if lst.Length = 0
    then 
         [||]
    else
        let n = lst.Length
        let array = Array.zeroCreate n
        for i in 0 .. n - 1 do
            array.[i] <- lst.Item(i)
        array
                    
mainConvert [] |> printfn "array = %A"   
mainConvert [1; 2; 3] |> printfn "array = %A"  