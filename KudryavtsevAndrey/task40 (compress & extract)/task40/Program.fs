let extract x =
    let arrByte = Array.create 4 0uy
    arrByte.[0] <- byte (x &&& 0x000000ff) 
    arrByte.[1] <- byte ((x &&& 0x0000ff00) >>> 8)
    arrByte.[2] <- byte ((x &&& 0x00ff0000) >>> 16)
    arrByte.[3] <- byte ((x &&& 0xff000000) >>> 24)
    arrByte

printfn "%A" (extract 2281448)
printfn "%A" (System.BitConverter.GetBytes 2281448)


let compress (byteArr: array<byte>) = 
    let intArr = byteArr |> Array.map int    
    let y = 0 + int intArr.[0]  + (intArr.[1] <<< 8) + (intArr.[2] <<< 16) + (intArr.[3] <<< 24)
    y
    
printfn "%A" (compress [|187uy; 228uy; 14uy; 88uy|])  
printfn "%A" (System.BitConverter.ToInt32 ([|187uy; 228uy; 14uy; 88uy|], 0))

printfn "%A" (compress (extract 1234567890))
