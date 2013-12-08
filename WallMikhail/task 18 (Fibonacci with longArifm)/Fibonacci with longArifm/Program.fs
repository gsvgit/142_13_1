module fibLong

type Long =
    | Num of list<int>
    static member (++) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num lst1, Num lst2 -> Num (List.tail (Addit.main (1::lst1) (1::lst2)))
    static member (+*+) ((l1:Long), (l2:Long)) =
        match l1, l2 with 
        | Num lst1, Num lst2 -> Num (List.tail (Multi.main (1::lst1) (1::lst2)))

let rec fibRec n =    
    if n < 1
    then Num [0]
    elif n = 1 || n = 2
    then Num [1]
    else fibRec (n - 1) ++ fibRec (n - 2)         
      
let fibIter n =              
    let f1 = ref (Num [1])
    let f2 = ref (Num [1])
    let sum = ref (Num [0])
    let i = 3
    for i in 3..n do
        sum := !f1 ++ !f2
        f1 := !f2
        f2 := !sum
    !f2 

let fibIterWithRec n =
    let rec FibN f1 f2 k =
        if k = n
        then f2
        else FibN (f1 ++ f2) f1 (k + 1)
    FibN (Num [1]) (Num [1]) 1    
    
let fibMat n =
    let f1 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let f2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let f3 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    for i in 1..n - 1 do
            f1.[0] <- (f2.[0] +*+ f3.[0]) ++ (f2.[1] +*+ f3.[2])
            f1.[1] <- (f2.[0] +*+ f3.[1]) ++ (f2.[1] +*+ f3.[3])
            f1.[2] <- (f2.[2] +*+ f3.[0]) ++ (f2.[3] +*+ f3.[2])
            f1.[3] <- (f2.[2] +*+ f3.[1]) ++ (f2.[3] +*+ f3.[3])
            f2.[0] <- f1.[0]
            f2.[1] <- f1.[1]
            f2.[2] <- f1.[2]
            f2.[3] <- f1.[3]
    f1.[2]
    
let fibMatLog n =
    if n = 1 || n = 2
    then Num [1]
    else 
        let rec fib k =
            let mat = [|Num [1]; Num [1]; Num [1]; Num[0]|]
            let multi (f2:array<_>) (f3:array<_>) =
                let f1 = [|Num [0]; Num [0]; Num [0]; Num [0]|]
                f1.[0] <- (f2.[0] +*+ f3.[0]) ++ (f2.[1] +*+ f3.[2])
                f1.[1] <- (f2.[0] +*+ f3.[1]) ++ (f2.[1] +*+ f3.[3])
                f1.[2] <- (f2.[2] +*+ f3.[0]) ++ (f2.[3] +*+ f3.[2])
                f1.[3] <- (f2.[2] +*+ f3.[1]) ++ (f2.[3] +*+ f3.[3])
                f1
            if k = 2
            then multi mat mat
            elif k % 2 = 1
                then multi (fib (k - 1)) mat
                else 
                    let array = fib (k / 2)
                    multi array array
        let fibN (f4:array<_>) = 
             f4.[2]
        fibN (fib n)

let fibArr n =  
    let arr = Array.create n (Num [0])        
    arr.[0] <- Num [1]
    arr.[1] <- Num [1]
    for i in 2..n - 1  do
        arr.[i] <- arr.[i - 2] ++ arr.[i - 1]
    arr   
    
printfn "fibRec = %A" (fibRec 10)
printfn "fibIter = %A" (fibIter 20)
printfn "fibIterWithRec = %A" (fibIterWithRec 20)
printfn "fibMat = %A" (fibMat 20)
printfn "fibMatLog = %A" (fibMatLog 20)
printfn "fibArr = %A" (fibArr 8)
