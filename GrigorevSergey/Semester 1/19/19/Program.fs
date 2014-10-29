namespace Problem

open System

module Main =
    let main (f : float) =
        let bits (arr : array<byte>) =
            let l = Array.length arr
            let a = Array.zeroCreate l
            let rec iter n (acc : string) =
                if n = 0
                then
                    if acc.Length = 8
                    then acc
                    else iter 0 ("0" + acc)
                else iter (n / 2) ((n % 2).ToString () + acc)
            for i in 0 .. l - 1 do
                a.[i] <- iter ((int) arr.[i]) ""
            String.Join (" ", a |> Array.rev)
        let res = BitConverter.GetBytes (f) |> bits
        res |> printfn "%A"
        res