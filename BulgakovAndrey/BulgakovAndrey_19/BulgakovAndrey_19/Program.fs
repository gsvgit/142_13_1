let main (x:float) =
    let ar = System.BitConverter.GetBytes x
    let ar2 = Array.zeroCreate 8
    printfn "%A" ar
    for i in ar do
        let ar2 = Array.zeroCreate 8
        let j = ref 0
        while !j < 8 do
            if (i >>> !j) % 2uy = 1uy
            then
                printf "1"
            else 
                printf "0"
            j := !j + 1
        
    
main (1.2345)