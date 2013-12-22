let main arr1 arr2 =
    let rec sum (arr: int array) i =
        if i = 0
        then arr.[0]
        else sum arr (i - 1) + arr.[i]
    let rec summing (arr1: int array) (arr2: int array) i =
        if i > -1 
        then   
            if arr1.[i] >= 0 && arr1.[i] < arr2.Length 
            then sum arr2 arr1.[i] + summing arr1 arr2 (i - 1)
            else sum arr2 (arr2.Length - 1) + summing arr1 arr2 (i - 1)
        else 0
    summing arr1 arr2 (arr1.Length - 1)
let array1 = [|1; 5; 5; 3|]
let array2 = [|6; 4; 1; 2; 3; 8|]
main array1 array2 |> printfn "%A"



