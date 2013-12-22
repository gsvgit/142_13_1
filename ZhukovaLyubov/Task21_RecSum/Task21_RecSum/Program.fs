let mainSum arr1 arr2 =
    let rec sum (arr: array<int>) j = 
        if j >= 0 
        then arr.[j] + sum arr (j - 1)
        else 0
    let rec sum2 (arr1: array<int>) (arr2: array<int>) i =
        if i <= arr1.Length - 1 
        then
            if arr1.[i] >= 0 && arr1.[i] < arr2.Length
            then sum arr2 arr1.[i] + sum2 arr1 arr2 (i + 1) 
            else sum arr2 (arr2.Length - 1) + sum2 arr1 arr2 (i + 1)
        else 0
    sum2 arr1 arr2 0

printfn "%A" (mainSum [|1; 2; 3|] [|4; 5|])
