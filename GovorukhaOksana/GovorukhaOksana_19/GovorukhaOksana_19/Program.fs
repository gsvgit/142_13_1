let f (x: float) =
    let byte = System.BitConverter.GetBytes(x)
    let (b:byte) = 1uy
    for i in 0..byte.Length - 1 do 
        for j in 0 .. 7 do
            printf "%A" (int((byte.[i] >>> j) &&& b))
    printfn ""
    
f -3.13
f 0.05
