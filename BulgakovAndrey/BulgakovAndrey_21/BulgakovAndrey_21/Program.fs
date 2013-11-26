let mainSum ar1 ar2 =
    let rec sum (a:int[]) j=
        if j>=0 then a.[j] + sum a (j - 1)
        else 0
    
    let rec sum2 (ar1:int[]) (ar2:int[]) i =
        if i<= ar1.Length - 1 then
            if ar1.[i] >=0 && ar1.[i] < Array.length ar1
            then (sum ar2 ar1.[i]) + (sum2 ar1 ar2 (i+1)) 
            else (sum ar2 (ar2.Length - 1)) + (sum2 ar1 ar2 (i+1))
        else 0
    sum2 ar1 ar1 0
printfn "%A" (mainSum [|1;2;3|] [|1;2;3|])