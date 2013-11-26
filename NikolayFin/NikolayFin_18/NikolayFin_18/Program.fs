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
        | Num lst1, Num lst2 -> Num (Multiplication.main (1 :: lst1) (1 :: lst2) |> List.tail)     
let rec mainRec fibN =
    if fibN < 1
    then failwith "An invalid value FibN"
    else  
        if fibN = 1 || fibN = 2
        then Num [1] 
        else mainRec (fibN - 1) ++ mainRec (fibN - 2)
printfn ""
printfn "Recursive method" 
printfn ""
printfn "1. %A" (mainRec 25)
printfn "2. %A" (mainRec 1)
let mainIt fibN =
    if fibN < 1
    then failwith "An invalid value FibN"
    else  
        let fibN1 = ref (Num [1])
        let fibN2 = ref (Num [1])
        for i in 1 .. fibN - 2 do
            fibN2 := !fibN2 ++ !fibN1
            fibN1 := !fibN2 -- !fibN1
        !fibN2
printfn ""
printfn "Iterative method" 
printfn ""
printfn "1. %A" (mainRec 25)
printfn "2. %A" (mainRec 1)
let mainRecIt fibN =
    if fibN < 1
    then failwith "An invalid value FibN"
    else  
        if fibN = 1 || fibN = 2
        then Num [1] 
        else
            let rec fib fibN1 fibN2 i =
                if i = fibN 
                then fibN1
                else fib (fibN1 ++ fibN2) fibN1 (i + 1)               
            fib (Num [1]) (Num [1]) 2 
printfn ""
printfn "Iterative me1thod without ref" 
printfn ""
printfn "1. %A" (mainRecIt 25)
printfn "2. %A" (mainRecIt 1)
let mainMatr n =
    if n < 1
    then failwith "An invalid value FibN"
    else
        if n = 1
        then Num [1]
        else        
            let fibN = Array.zeroCreate 4
            let fibN1 = [|Num [1]; Num [1]; Num [1]; Num [0]|] 
            let fibN2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
            for i in 1 .. n - 1 do
                fibN.[0] <- (fibN1.[0] +* fibN2.[0]) ++ (fibN1.[1] +* fibN2.[2])
                fibN.[1] <- (fibN1.[0] +* fibN2.[1]) ++ (fibN1.[1] +* fibN2.[3])
                fibN.[2] <- (fibN1.[2] +* fibN2.[0]) ++ (fibN1.[3] +* fibN2.[2])
                fibN.[3] <- (fibN1.[2] +* fibN2.[1]) ++ (fibN1.[3] +* fibN2.[3])
                fibN1.[0] <- fibN.[0] 
                fibN1.[1] <- fibN.[1] 
                fibN1.[2] <- fibN.[2]
                fibN1.[3] <- fibN.[3]
            fibN.[1]  
printfn ""
printfn "Matrix" 
printfn ""
printfn "1. %A" (mainMatr 25)
printfn "2. %A" (mainMatr 1)
let mainMatrix n =
    if n < 1
    then failwith "An invalid value FibN"
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
                else
                    if n % 2 = 0
                    then 
                        let matrixEven = fib (n / 2)
                        multiplicate matrixEven matrixEven
                    else multiplicate (fib (n - 1)) matrix 
            let number (fibNn:array<_>) = fibNn.[1]
            number (fib n)
printfn ""
printfn "Matrix (log)" 
printfn ""
printfn "1. %A" (mainMatrix 25)
printfn "2. %A" (mainMatrix 1)
let mainMatLog n =
    if n < 1
    then failwith "An invalid value FibN"
    else 
        if n = 1
        then [|Num [1]|]
        else
            let fibN = Array.zeroCreate n
            fibN.[0] <- Num [1]
            fibN.[1] <- Num [1]
            for i in 2 .. n - 1 do
                fibN.[i] <- fibN.[i - 1] ++ fibN.[i - 2]
            fibN
printfn ""
printfn "Computation from 1 to n" 
printfn ""
printfn "1. %A" (mainMatLog 1)
printfn "2. %A" (mainMatLog 2)