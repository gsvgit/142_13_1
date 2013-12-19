let mainSum arr1 arr2 = 
   let rec sum1 (arr3: array<int>) j = 
        if j >= 0 
        then arr3.[j] + sum1 arr3 (j - 1)
        else 0
   let rec sum2 (arr1: array<int>) (arr2: array<int>) i =
       if i <= arr1.Length - 1
       then
           if arr1.[i] >=0 && arr1.[i] < Array.length arr1
           then sum1 arr2 arr1.[i] + sum2 arr1 arr2 (i + 1)
           else sum1 arr2 (arr2.Length - 1) + sum2 arr1 arr2 (i + 1)
       else 0
   sum2 arr1 arr1 0

mainSum [|4; 5; 2; 1|] [|0; 3|] |> printfn "%A" 
