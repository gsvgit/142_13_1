let mainSum arr1 arr2 = 
    let rec amount (arr:array<_>) n =
        if n >= 0 
        then arr.[n] + amount arr (n - 1)
        else 0     
    let rec sum (arr1:array<_>) (arr2:array<_>) i =
        if i < arr1.Length 
        then
            if arr1.[i] >= 0 && arr1.[i] < arr2.Length
            then amount arr2 arr1.[i] + sum arr1 arr2 (i + 1)
            else amount arr2 (arr2.Length - 1) + sum arr1 arr2 (i + 1)
        else 0
    sum arr1 arr2 0
printfn "1. %A" (mainSum [|1; 1; 1|] [|1; 2; 3|])
printfn "2. %A" (mainSum [|2; 2; 2|] [|1; 2; 3|])
printfn "3. %A" (mainSum [|3; 3; 3|] [|1; 2; 3|])
printfn "4. %A" (mainSum [|1; 2; 3; 4|] [|1; 2; 3|])