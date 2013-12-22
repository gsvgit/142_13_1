open Addition
open Multiplication

type Long =
    | Num of list<int>
    static member (++) (lst1, lst2) = 
        match lst1, lst2 with
        | Num lst1, Num lst2 -> Num (Addition.main lst1 lst2)
    static member (--) (lst1, lst2) = 
        match lst1, lst2 with
        | Num lst1, Num lst2 -> Num (Addition.main lst1 (((List.head lst2) * -1) :: (List.tail lst2)))
    static member (+*) (lst1, lst2) =
        match lst1, lst2 with
        | Num lst1, Num lst2 -> Num (Multiplication.main (1 :: lst1) (1 :: lst2) |> List.tail)

let rec recursion n =
    if n <= 0
    then failwith "Incorrect index"
    elif n = 1 || n = 2 
    then Num [1]
    else recursion (n - 2) ++ recursion (n - 1)
printfn "Recursion = %A" (recursion 13)

let iterat n =
    if n <= 0
    then failwith "Incorrect index"
    else
        let f1 = ref (Num [1])
        let f2 = ref (Num [1])
        for i in 1..n - 2 do
            f2 := !f2 ++ !f1
            f1 := !f2 -- !f1 
        !f2     
printfn "Iteration = %A" (iterat 13)

let itern n =
    if n <= 0
    then failwith "Incorrect index"
    else  
        if n = 1 || n = 2
        then Num [1] 
        else
            let rec fib n1 n2 i =
                if i = n 
                then n1
                else fib (n1 ++ n2) n1 (i + 1)               
            fib (Num [1]) (Num [1]) 2
printfn "Iteration with recursion = %A" (itern 13)  
   
let matr n =
    let n1 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let n2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let n3 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    if n > 0 
    then
        for i in 1..n - 1 do
            n1.[0] <- (n2.[0] +* n3.[0]) ++ (n2.[1] +* n3.[2])
            n1.[1] <- (n2.[0] +* n3.[1]) ++ (n2.[1] +* n3.[3])
            n1.[2] <- (n2.[2] +* n3.[0]) ++ (n2.[3] +* n3.[2])
            n1.[3] <- (n2.[2] +* n3.[1]) ++ (n2.[3] +* n3.[3])
            n2.[0] <- n1.[0]
            n2.[1] <- n1.[1]
            n2.[2] <- n1.[2]
            n2.[3] <- n1.[3]
        n1.[2]
    else 
        failwith "Incorrect index"
printfn "Matrix = %A" (matr 13)

let matlg n =
    if n < 1
    then failwith "Incorrect index"
    else  
        if n = 1 
        then Num [1] 
        else
            let matrix = [|Num [1]; Num [1]; Num [1]; Num [0]|]
            let rec fib n =
                let arr = Array.zeroCreate 4
                let multi (arr1 : array<_>) (arr2 : array<_>) =
                    arr.[0] <- ((arr1.[0] +* arr2.[0]) ++ (arr1.[1] +* arr2.[2]))
                    arr.[1] <- ((arr1.[0] +* arr2.[1]) ++ (arr1.[1] +* arr2.[3]))
                    arr.[2] <- ((arr1.[2] +* arr2.[0]) ++ (arr1.[3] +* arr2.[2]))
                    arr.[3] <- ((arr1.[2] +* arr2.[1]) ++ (arr1.[3] +* arr2.[3]))
                    arr                
                if n = 2 
                then multi matrix matrix
                elif n % 2 = 0
                then 
                    let matrix1 = fib (n / 2)
                    multi matrix1 matrix1
                else multi (fib (n - 1)) matrix 
            let res (resarr : array<_>) = resarr.[1]
            res (fib n)
printfn "Matrix (Log) = %A" (matlg 13)

let allfib n =
    if n <= 0
    then failwith "Incorrect index"
    else 
        if n = 1
        then [|Num [1]|]
        else
            let arr = Array.zeroCreate n
            arr.[0] <- Num [1]
            arr.[1] <- Num [1]
            for i in 2..n - 1 do
                arr.[i] <- arr.[i - 1] ++ arr.[i - 2]
            arr
printfn "All Fib till N = %A" (allfib 13)
