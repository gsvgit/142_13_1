(*let mainSum arr1 arr2 =
    let res = ref 0
    for i in arr1 do
        let n = 
            if i >= 0 && i < Array.length arr2
            then i 
            else arr2.Length - 1
        for j in 0..n do
            res := !res + arr2.[j]
    !res
printfn "%A" (mainSum [|1; 2; 3; 21; 9|] [|13; 45; 7|])*)



