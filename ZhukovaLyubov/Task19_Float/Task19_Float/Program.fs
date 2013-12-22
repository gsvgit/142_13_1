let convert (x: float) =
    let arByte = System.BitConverter.GetBytes x
    let b = 1uy
    for i in 0 .. arByte.Length - 1 do
        for j in 0 .. 7 do
            printf "%A" (int((arByte.[i] >>> j) &&& b))
    printfn ""
convert 13.13
convert 0.15