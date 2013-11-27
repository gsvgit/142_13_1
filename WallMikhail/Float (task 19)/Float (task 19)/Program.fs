let main (f: float) =

    let byte = System.BitConverter.GetBytes(f)
    let (b: byte) = 1uy    
    for i in 0..byte.Length - 1 do
        for j in 0..7 do
            printf "%A" (int ((byte.[i] >>> j) &&& b))
    printfn ""

main (54.24)
main (-1.27910)
main (0.0)