type Long =
    | Num of list<int>
    static member (++) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num l1, Num l2 -> Num (Addition.main l1 l2)
    static member (--) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num l1, Num l2 -> Num (Addition.main l1 (((List.head l2) * -1) :: (List.tail l2)))
    static member (-*-) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num l1, Num l2 -> Num (List.tail(Multi.main (1 :: l1) (1 :: l2)))
//Recursive
let rec recurs n =
    if n = 1 || n = 2 
    then Num [1]
    elif n <= 0 
    then failwith "Error. Please enter the correct index."
    else recurs (n - 2) ++ recurs (n - 1)

//Iterative
let iterat n =
    if n <= 0
    then failwith "Error. Please enter the correct index."
    elif n = 1 || n = 2
    then Num [1] 
    else
        let f = ref (Num [1])
        let f1 = ref (Num [1])
        for i in 2..n do
            f1 := !f -- !f1
            f := !f ++ !f1 
        !f     
//Iterative without ref and mut
let itref n =
    if n > 0 
    then
     let rec fib n0 n1 i = 
         if i = n
         then n1
         else fib (n0 ++ n1) n0 (i + 1)
     fib (Num [1]) (Num [1]) 1
    else failwith "Error. Please enter the correct index."
//Mat
let mat n =
  if n < 1
  then failwith "Error. Please enter the correct index."
  else
    let arr = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let arr1 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let arr2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    for i in 1..n do
        arr1.[0] <- (arr.[0] -*- arr2.[0]) ++ (arr.[1] -*- arr2.[2])
        arr1.[1] <- (arr.[0] -*- arr2.[1]) ++ (arr.[1] -*- arr2.[3])
        arr1.[2] <- (arr.[2] -*- arr2.[0]) ++ (arr.[3] -*- arr2.[2])
        arr1.[3] <- (arr.[2] -*- arr2.[1]) ++ (arr.[3] -*- arr2.[3])
        arr.[0] <- arr1.[0]
        arr.[1] <- arr1.[1]
        arr.[2] <- arr1.[2]
        arr.[3] <- arr1.[3]   
    arr.[3]
//Mat log
let matlog x =
  if x = 1
  then Num [1]
  elif x < 1
  then failwith "Error. Please enter the correct index."
  else
      let rec fib n =
        let std = [|Num [1]; Num [1]; Num [1]; Num [0]|]    
        let step (arr1:array<_>) (arr2:array<_>) =
            let arr = [|Num [0]; Num [0]; Num [0]; Num [0]|]
            arr.[0] <- (arr1.[0] -*- arr2.[0]) ++ (arr1.[1] -*- arr2.[2])
            arr.[1] <- (arr1.[0] -*- arr2.[1]) ++ (arr1.[1] -*- arr2.[3])
            arr.[2] <- (arr1.[2] -*- arr2.[0]) ++ (arr1.[3] -*- arr2.[2])
            arr.[3] <- (arr1.[2] -*- arr2.[1]) ++ (arr1.[3] -*- arr2.[3])
            arr
        if n = 2
        then step std std
        elif n % 2 = 1
        then step (fib (n - 1)) std
        else 
            let array = fib (n /2) 
            step array array
      let take (arr3:array<_>) = 
                arr3.[2]
      take (fib x)

//Array
let arrs n = 
    if n <= 0 
    then failwith "Error. Please enter the correct index."
    elif n = 1
    then [|Num [1]|]
    else
        let outArray = Array.zeroCreate n    
        outArray.[0] <- Num [1] 
        outArray.[1] <- Num [1]
        for i in 2..n - 1 do
            outArray.[i] <- outArray.[i - 1] ++ outArray.[i - 2]
        outArray
printfn "fibRec = %A" (recurs 20)
printfn "fibIter = %A" (iterat 20)
printfn "fibIterWithRec = %A" (itref 20)
printfn "fibMat = %A" (mat 20)
printfn "fibMatLog = %A" (matlog 20)
printfn "fibArr = %A" (arrs 8)
