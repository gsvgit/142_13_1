
let mainSum (arr1 : int array) (arr2 : int array) =
    let res = ref 0
    for i in arr1 do
        let n = 
            if i >=0 && i < Array.length arr2
            then i 
            else arr2.Length - 1
        for j in 0..n do
            res := !res + arr2.[j]
    !res
printfn "res1 = %A" (mainSum [|1; 1; 1|] [|1; 2; 3|])

let rec mainSumRec (arr1 : int array) (arr2 : int array) = 
    
    let rec sum (arr : int array) k = 
        if k >= 0
        then arr.[k] + sum arr (k - 1)
        else 0 

    let rec summ (arr1 : int array) (arr2 : int array) l = 
        if l < arr1.Length
        then 
            if arr1.[l] >= 0 && l < arr2.Length 
            then sum arr2 arr1.[l] + summ arr1 arr2 (l + 1) 
            else sum arr2 (arr2.Length - 1) + summ arr1 arr2 (l + 1) 
        else 0
    summ arr1 arr2 0

printfn "res2 = %A" (mainSumRec [|1; 1; 1|] [|1; 2; 3|])

let arr1 = [|3; 4; 5|]
let arr2 = [|2; 1; 7; 8; 8; 9|]

printfn "mainSum = %A" (mainSum arr1 arr2)
printfn "mainSumRecursive = %A" (mainSumRec arr1 arr2)