open Addition
open Mult

type Long =
    | Num of list<int>
    static member (++) ((l1:Long), (l2:Long)) =
        match l1,l2 with
        | Num lst1, Num lst2 -> Num (Addition.main (1 :: lst1) (1 :: lst2) |> List.tail) 
    static member (--) ((l1:Long), (l2:Long)) =
        match l1, l2 with
        | Num lst1, Num lst2 -> Num (Addition.main (1 :: lst1) (-1 :: lst2) |> List.tail)  
    static member (+*) ((l1:Long), (l2:Long)) =
        match l1,l2 with
        | Num lst1, Num lst2 -> Num (Mult.multiplication (1 :: lst1) (1 :: lst2) |> List.tail)     


let rec mainRec fibN =
    if fibN < 1
    then failwith "Error! Wrong index!"
    elif fibN = 1 || fibN = 2
    then Num [1] 
    else mainRec (fibN - 1) ++ mainRec (fibN - 2)
printfn "Recursive way" 
printfn "1. %A" (mainRec 100)
printfn "2. %A" (mainRec 10)


let mainIter fibN =
    if fibN < 1
    then failwith "Error! Wrong index!"
    else  
        let fibN1 = ref (Num [1])
        let fibN2 = ref (Num [1])
        for i in 2..fibN do
            fibN2 := !fibN1 ++ !fibN2
            fibN1 := !fibN2 -- !fibN1
        !fibN1 
printfn "Iterative way" 
printfn "1. %A" (mainIter 20)
printfn "2. %A" (mainIter 11)


let mainRecIt fibN =
    if fibN < 1
    then failwith "Error! Wrong index!"
    else  
        if fibN = 1 || fibN = 2
        then Num [1] 
        else
            let rec fib fibN1 fibN2 i =
                if i = fibN 
                then fibN1
                else fib (fibN1 ++ fibN2) fibN1 (i + 1)               
            fib (Num [1]) (Num [1]) 2 
printfn "Iterative/Recursive way" 
printfn "1. %A" (mainRecIt 10)
printfn "2. %A" (mainRecIt 5)


let mainMatr n =
    let fibN = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let fibN1 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let fibN2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
  
    if n > 0 
    then
        for i in 1..n - 1 do
            fibN.[0] <- (fibN1.[0] +* fibN2.[0]) ++ (fibN1.[1] +* fibN2.[2])
            fibN.[1] <- (fibN1.[0] +* fibN2.[1]) ++ (fibN1.[1] +* fibN2.[3])
            fibN.[2] <- (fibN1.[2] +* fibN2.[0]) ++ (fibN1.[3] +* fibN2.[2])
            fibN.[3] <- (fibN1.[2] +* fibN2.[1]) ++ (fibN1.[3] +* fibN2.[3])
            fibN1.[0] <- fibN.[0]
            fibN1.[1] <- fibN.[1]
            fibN1.[2] <- fibN.[2]
            fibN1.[3] <- fibN.[3]
        fibN.[2]
    else 
        failwith "Error! Wrong index!"
printfn "Matrix method" 
printfn "1. %A" (mainMatr 10)
printfn "2. %A" (mainMatr 5)


let mainMatrix n =
    if n < 1
    then failwith "Error! Wrong index!"
    else  
        if n = 1 
        then Num [1] 
        else
            let matrix = [|Num [1]; Num [1]; Num [1]; Num [0]|]
            let rec fib n =
                let fibN = Array.zeroCreate 4
                let multiplicate (fibN1:array<_>) (fibN2:array<_>) =
                    fibN.[0] <- (fibN1.[0] +* fibN2.[0]) ++ (fibN1.[1] +* fibN2.[2])
                    fibN.[1] <- (fibN1.[0] +* fibN2.[1]) ++ (fibN1.[1] +* fibN2.[3])
                    fibN.[2] <- (fibN1.[2] +* fibN2.[0]) ++ (fibN1.[3] +* fibN2.[2])
                    fibN.[3] <- (fibN1.[2] +* fibN2.[1]) ++ (fibN1.[3] +* fibN2.[3])
                    fibN                
                if n = 2 
                then multiplicate matrix matrix
                elif n % 2 = 0
                then 
                    let matrixEven = fib (n / 2)
                    multiplicate matrixEven matrixEven
                else multiplicate (fib (n - 1)) matrix 
            let number (fibNn:array<_>) = fibNn.[1]
            number (fib n)
printfn "Log matrix" 
printfn "1. %A" (mainMatrix 10)
printfn "2. %A" (mainMatrix 5)


let mainFibNumbers n =
    if n < 1
    then failwith "Error! Wrong index!"
    elif n = 1
    then [|Num [1]|]
    else
        let fibN = Array.zeroCreate n
        fibN.[0] <- Num [1]
        fibN.[1] <- Num [1]
        for i in 2 .. n - 1 do
            fibN.[i] <- fibN.[i - 1] ++ fibN.[i - 2]
        fibN
printfn "Massive: " 
printfn "1. %A" (mainFibNumbers 5)
printfn "2. %A" (mainFibNumbers 10)
