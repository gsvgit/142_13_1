let extract x =
    let arrbyte = Array.create 4 0uy
    arrbyte.[0] <- byte (x &&& 0x000000ff) 
    arrbyte.[1] <- byte ((x &&& 0x0000ff00) >>> 8)
    arrbyte.[2] <- byte ((x &&& 0x00ff0000) >>> 16)
    arrbyte.[3] <- byte ((x &&& 0xff000000) >>> 24)
    arrbyte.[0], arrbyte.[1], arrbyte.[2], arrbyte.[3] 
   
let compress (byte1, byte2, byte3, byte4) = 
    let bytearr = Array.create 4 0uy
    bytearr.[0] <- byte1
    bytearr.[1] <- byte2
    bytearr.[2] <- byte3
    bytearr.[3] <- byte4
    let intArr = [|for i in 0..3 -> int bytearr.[i]|]    
    let res = 0 + int intArr.[0]  + (intArr.[1] <<< 8) + (intArr.[2] <<< 16) + (intArr.[3] <<< 24)
    res
    
printfn "%A" (extract 1312119)
printfn "%A" (System.BitConverter.GetBytes 1312119)

printfn "%A" (compress (13uy, 12uy, 11uy, 9uy))
printfn "%A" (System.BitConverter.ToInt32 ([|13uy; 12uy; 11uy; 9uy|], 0))

printfn "%A" (extract 1312119 |> compress)