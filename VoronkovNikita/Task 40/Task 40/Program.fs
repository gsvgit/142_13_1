let extract x =
    let arr = Array.create 4 0uy
    arr.[0] <- byte (x &&& 0x000000ff) 
    arr.[1] <- byte ((x &&& 0x0000ff00) >>> 8)
    arr.[2] <- byte ((x &&& 0x00ff0000) >>> 16)
    arr.[3] <- byte ((x &&& 0xff000000) >>> 24)
    arr

printfn "%A" (extract 12344321)
printfn "%A" (System.BitConverter.GetBytes 12344321)


let compress (arr: byte array) = 
    let intArr = arr |> Array.map int    
    let y = int intArr.[0]  + (intArr.[1] <<< 8) + (intArr.[2] <<< 16) + (intArr.[3] <<< 24)
    y
    
printfn "%A" (compress [|28uy; 234uy; 123uy; 13uy|])  
printfn "%A" (System.BitConverter.ToInt32 ([|28uy; 234uy; 123uy; 13uy|], 0))

printfn "%A" (compress (extract 192837465))