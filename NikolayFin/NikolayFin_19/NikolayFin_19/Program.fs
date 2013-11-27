let main (fl:float) =
    let arr = System.BitConverter.GetBytes fl   
    for i in arr do       
        let j = ref 0
        while !j < 8 do
            if (i >>> !j) % 2uy = 1uy
            then
                printf "1"
            else 
                printf "0"
            j := !j + 1   
printfn "12.34"
(main 12.34)
printfn ""
printfn "-12.34"
(main -12.34)
printfn ""
printfn "0"
(main 0.00)
printfn ""
