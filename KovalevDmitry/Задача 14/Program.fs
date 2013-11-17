let arr1 = [|-1; 45; 1; 0; -97; 123; 31|]
let arr2 = [|-1000; 0; 0; 0; -890; -3|]

let main (inArr: int array) =
    let k = ref 0
    for i in 0..inArr.Length - 2 do
        for j in 0..inArr.Length - 2 do
        if inArr.[j] > inArr.[j + 1] 
        then
            k := inArr.[j]
            inArr.[j] <- inArr.[j + 1]
            inArr.[j + 1] <- !k
    inArr

printfn "%A" (main arr1)
printfn  "%A" (main arr2)
