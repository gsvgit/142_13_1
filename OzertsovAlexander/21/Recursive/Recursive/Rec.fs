let main (arr1:array<_>) (arr2:array<_>) = 
    let rec summa (arr3:array<_>) p =
        if p > -1
        then arr3.[p] + summa arr3 (p - 1)
        else 0
    
    let rec Sum (arr1:array<_>) (arr2:array<_>) i =
        if i <= (arr1.Length - 1)
        then
            if arr1.[i] >= 0 && arr1.[i] < Array.length arr2
            then (summa arr2 arr1.[i]) + (Sum arr1 arr2 (i+1))
            else (summa arr2 (arr2.Length - 1)) + (Sum arr1 arr2 (i+1))
        else 0
    Sum arr1 arr2 0

printfn "%A" (main    [|1; 3; 5; 7; 14; 68|] [|12; 13; 14; 1; 3|])
