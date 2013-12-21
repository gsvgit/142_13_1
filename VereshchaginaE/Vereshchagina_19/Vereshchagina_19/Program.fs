let main (fl : float) =
    let arr = System.BitConverter.GetBytes fl   
    for i in arr do       
        for j in 0..7 do
            if (i >>> j) % 2uy = 1uy
            then
                printf "1"
            else 
                printf "0"
               

printfn "0.34756587"
(main 0.34756587)


