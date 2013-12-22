let mainSum1 arr1 arr2 =
   
    let res = ref 0
    for i in arr1 do
        let n = 
            if i >= 0 && i < Array.length arr2
            then i 
            else arr2.Length - 1
        for j in 0..n do
            res := !res + arr2.[j]
    !res

let mainSum arr1 arr2 =

    let rec sum (a : array<int>) j =
        if j >= 0 
        then a.[j] + sum a (j - 1)
        else 0
    
    let rec count (arr1 : array<int>) (arr2 : array<int>) i =
        if i <= arr1.Length - 1 
        then
            if (arr1.[i] >= 0) && (arr1.[i] < Array.length arr1)
            then sum arr2 arr1.[i] + count arr1 arr2 (i + 1) 
            else sum arr2 (arr2.Length - 1) + count arr1 arr2 (i + 1)
        else 0
    count arr1 arr2 0

printfn "%A" (mainSum [|1; 3; 1; 2; 1; 1; 9|] [|10; 20; 30; 40|])
printfn "%A" (mainSum1 [|1; 3; 1; 2; 1; 1; 9|] [|10; 20; 30; 40|])