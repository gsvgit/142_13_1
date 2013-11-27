let main (f: float) =
    
    printfn "%A" f
    printfn ""

    let arrByte = System.BitConverter.GetBytes(f) 
    printfn "%A" arrByte
    printfn ""        

    let (b: byte) = 1uy    
    
    let (arrBit: byte array) = Array.create 8 0uy
    
    for i in 0..arrByte.Length - 1 do
        for j in 0..7 do
            printf "%A" (int ((arrByte.[i] >>> j) &&& b))                      
    printfn ""
   
    printfn "------------------"

main (65.578)
main (-545.8787)
main (0.0)
