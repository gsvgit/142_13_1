let convert (a: float) = 
    let arrByte = System.BitConverter.GetBytes(a)
    let (b:byte) = 1uy
    let (byteArr:byte array) = Array.create 8 0uy
    for i in 0..arrByte.Length - 1 do
        for j in 0..7 do
            printf "%A" (int ((arrByte.[i] >>> j) &&& b))
    printfn "" 
    printfn ""
convert (64.64)
convert (176.994)
convert (0.0)
