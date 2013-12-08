module StringHash

let stringHash (str:string) = 

    let sum = ref 0

    if str = "" 
    then 0
    else
        for i in 0 .. str.Length - 1 do
            sum := !sum + int str.[i] * (i + 1) 
        sum := !sum / str.Length 
        !sum

printfn "%A" (stringHash "NikolayFin_29")
printfn "%A" (stringHash "abc")
printfn "%A" (stringHash "b")
printfn "%A" (stringHash "")

    