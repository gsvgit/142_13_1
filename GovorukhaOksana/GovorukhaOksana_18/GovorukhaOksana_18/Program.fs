open Addition
open Multiplication

type Long =
    | Num of list<int>
    static member (++) (lst, lst2) =
        match lst, lst2 with
        | Num list, Num list2 -> Num (Addition.main (1 :: list) (1 :: list2) |> List.tail)
    static member (+*) (lst, lst2) = 
        match lst, lst2 with
        | Num list, Num list2 -> Num (Multiplication.main (1 :: list) (1 :: list2) |> List.tail)

let rec f7 x = 
    if x < 1
    then failwith "error. incorrect input"
    elif x = 1 || x = 2
    then Num [1]
    else f7 (x - 1) ++ f7 (x - 2)


let f8 x =
    if x < 1
    then failwith "error. incorrect input"
    else 
        let n1 = ref (Num [1])
        let n2 = ref (Num [1])
        let n = ref (Num [0])
        for i in 3 .. x do
            n := !n1 ++ !n2
            n1 := !n2
            n2 := !n
        !n2

let f9 x =
    if x < 1
    then failwith "error. incorrect input"
    else    
        let rec f n1 n2 i =
            if i = x
            then n2  
            else f (n1 ++ n2) n1 (i + 1) 
        f (Num [1]) (Num [1]) 1 
        
let f10 x = 
    if x < 1 
    then failwith "error. incorrect input"
    else
        let m1 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
        let m2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
        let m3 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
        for i in 1 .. x - 1 do
            m1.[0] <- (m2.[0] +* m3.[0]) ++ (m2.[1] +* m3.[2])
            m1.[1] <- (m2.[0] +* m3.[1]) ++ (m2.[1] +* m3.[3])
            m1.[2] <- (m2.[2] +* m3.[0]) ++ (m2.[3] +* m3.[2])
            m1.[3] <- (m2.[2] +* m3.[1]) ++ (m2.[3] +* m3.[3])
            m2.[0] <- m1.[0]
            m2.[1] <- m1.[1]
            m2.[2] <- m1.[2]
            m2.[3] <- m1.[3] 
        m1.[2]

let f11 x = 
    if x < 1 
    then failwith "error. incorrect input"
    elif x = 1 || x = 2
    then Num [1]
    else
        let rec f x =
            let mat = [|Num [1]; Num [1]; Num [1]; Num [0]|]
            let f2 (m1: array<_>) (m2: array<_>) =
                 let m = [|Num [0]; Num [0]; Num [0]; Num[0]|] 
                 m.[0] <- (m1.[0] +* m2.[0]) ++ (m1.[1] +* m2.[2])
                 m.[1] <- (m1.[0] +* m2.[1]) ++ (m1.[1] +* m2.[3])
                 m.[2] <- (m1.[2] +* m2.[0]) ++ (m1.[3] +* m2.[2])
                 m.[3] <- (m1.[2] +* m2.[1]) ++ (m1.[3] +* m2.[3])       
                 m
            if x = 2
            then f2 mat mat
            elif x % 2 = 1
            then
                f2 (f (x - 1)) mat 
            else 
                let m3 = f (x / 2)
                f2 m3 m3
        let f3 (arr: array<_>) =
            arr.[2]
        f3 (f x) 


let f12 x =
    if x < 1 
    then failwith "error. incorrect input"
    elif x = 1
    then [|Num [1]|]
    else
        let arr = Array.zeroCreate x
        arr.[0] <- Num [1]
        arr.[1] <- Num [1]
        for i in 2 .. x - 1 do
            arr.[i] <- arr.[i - 1] ++ arr.[i - 2]
        arr

f7 11 |> printfn "%A"
f8 11 |> printfn "%A"
f9 11 |> printfn "%A"
f10 11 |> printfn "%A"
f11 11 |> printfn "%A"
f12 11 |> printfn "%A"
