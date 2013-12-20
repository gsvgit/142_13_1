let compress a b c d =
    let byteArr = [|a; b; c; d|]
    System.BitConverter.ToInt32 (byteArr, 0)
let s = compress 12uy 34uy 56uy 234uy
printfn "%A" s

let extract (x: int32) = 
    System.BitConverter.GetBytes x
let k = extract 13
printfn "%A" k

printfn "%A" (extract s)