
let extract n =
    let array = Array.create 4 0uy
    array.[0] <- byte (n &&& 0x000000ff) 
    array.[1] <- byte ((n &&& 0x0000ff00) >>> 8)
    array.[2] <- byte ((n &&& 0x00ff0000) >>> 16)
    array.[3] <- byte ((n &&& 0xff000000) >>> 24)
    array.[0], array.[1], array.[2], array.[3]

let compress (n1, n2, n3, n4) = 
    let array = Array.create 4 0uy
    array.[0] <- n1
    array.[1] <- n2
    array.[2] <- n3
    array.[3] <- n4
    let array2 = [|for i in 0 .. 3 -> int array.[i]|]    
    let n = int array2.[0]  + (array2.[1] <<< 8) + (array2.[2] <<< 16) + (array2.[3] <<< 24)
    n

extract 2232421 |> printfn "%A" 
System.BitConverter.GetBytes 2232421 |> printfn "%A" 
    
compress (139uy, 24uy, 17uy, 123uy) |> printfn "%A" 
System.BitConverter.ToInt32 ([|139uy; 24uy; 17uy; 123uy|], 0) |> printfn "%A" 

extract 42932435|> compress |> printfn "%A" 
