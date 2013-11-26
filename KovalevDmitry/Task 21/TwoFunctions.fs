let mainSum arr1 arr2 =
   
    let res = ref 0
    for i in arr1 do
        let n = 
            if i >= 0 && i < Array.length arr2
            then i 
            else arr2.Length - 1
        for j in 0..n do
            res := !res + arr2.[j]
    !res

let mainSumRecursive arr1 arr2 =
     
     let rec sum (arr: int array) i =
        if i = 0
        then arr.[0]
        else sum arr (i - 1) + arr.[i]

     let rec summa (arr1: int array) (arr2: int array) i =
        if i > -1 
        then   
            if arr1.[i] >= 0 && arr1.[i] < arr2.Length 
            then sum arr2 arr1.[i] + summa arr1 arr2 (i - 1)
            else sum arr2 (arr2.Length - 1) + summa arr1 arr2 (i - 1)
        else 0
     
     summa arr1 arr2 (arr1.Length - 1)

let arr1 = [|3; 4; 5|]
let arr2 = [|2; 1; 7; 8; 8; 9|]

printfn "mainSum = %A" (mainSum arr1 arr2)
printfn "mainSumRecursive = %A" (mainSumRecursive arr1 arr2)