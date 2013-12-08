let mainSum arr1 arr2 =

    let rec sum (a:int[]) j =
        if j >= 0 then a.[j] + sum a (j - 1)
        else 0
    
    let rec Sum (arr1:int[]) (arr2:int[]) i =
        if i <= arr1.Length - 1 
        then
            if arr1.[i] >= 0 && arr1.[i] < Array.length arr1
            then sum arr2 arr1.[i] + Sum arr1 arr2 (i + 1) 
            else sum arr2 (arr2.Length - 1) + Sum arr1 arr2 (i + 1)
        else 0
    Sum arr1 arr1 0

printfn "%A" (mainSum [|3; 6; 7;|] [|1; 2; 1; 2|])
printfn "%A" (mainSum [|5; 2; 5; 1; 8|] [||])
