
let main (f: float) =
    
    printfn "%A" f

    let arrByte = System.BitConverter.GetBytes(f)      
    let b = 1uy    
    for i in 0..arrByte.Length - 1 do
        for j in 0..7 do
            printf "%A" (int (arrByte.[i] >>> j &&& b))                      

    printfn ""
main (23.855)
main (-13.7563675)
main (0.0)