let main arr1 arr2 = 

    let rec sum (arr: int array) i =
        if i = 0   
        then arr.[0]
        else sum arr (i - 1) + arr.[i]

    let rec Sum (arr1: int array) (arr2: int array) i =
        if i > -1
        then 
            if arr1.[i] >= 0 && arr1.[i] < Array.length arr2
            then (sum arr2 arr1.[i]) + (Sum arr1 arr2 (i - 1))
            else (sum arr2 (arr2.Length - 1)) + (Sum arr1 arr2 (i - 1))
        else 0

    Sum arr1 arr2 (arr1.Length - 1)

printfn "%A" (main [|5; 2; 5; 1; 8|] [|2; 6; 5|])
          