module FibLong 
    
type Long =
    | Num of list<int>
    static member (++) ((l1:Long), (l2:Long)) =
        match l1, l2 with
        | Num lst1, Num lst2 -> Num (List.tail (Addition.summa (1::lst1) (1::lst2)))
    static member (+*+) ((l1:Long), (l2:Long)) =
        match l1, l2 with
        | Num lst1, Num lst2 -> Num (List.tail (Multiplication.multList (1::lst1) (1::lst2))) 
        
let rec fibRec n =    
    if n < 1
    then Num [0]
    elif n = 1 || n = 2
    then Num [1]
    else fibRec(n - 1) ++ fibRec(n - 2)         
      
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

let fibIterRec n =
    let rec itFib f1 f2 i =
        if i = n
        then f2
        else itFib (f1 ++ f2) f1 (i + 1)
    itFib (Num [1]) (Num [1]) 1    
    
let fibMatrix n =
    let arr = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let arrF = [|Num [1]; Num [1]|]
    let arrHelp = [|Num [0]; Num [0]|]
    for i in 2..n do
        arrHelp.[0] <- (arr.[0] +*+ arrF.[0]) ++ (arr.[1] +*+ arrF.[1])
        arrHelp.[1] <- (arr.[2] +*+ arrF.[0]) ++ (arr.[3] +*+ arrF.[1])
        arrF.[0] <- arrHelp.[0]
        arrF.[1] <- arrHelp.[1]
    arrF.[1]
    
let fibMatrixLog n =
    if n = 1 || n = 2
    then Num [1]
    else 
        let rec fibMatrix k =
            let mx = [|Num [1]; Num [1]; Num [1]; Num [0]|]
            let sqrMatrix (m1: Long array) (m2: Long array) =
                let mRes = Array.create 4 (Num [0])      
                mRes.[0] <- (m1.[0] +*+ m2.[0]) ++ (m1.[1] +*+ m2.[2])
                mRes.[1] <- (m1.[0] +*+ m2.[1]) ++ (m1.[1] +*+ m2.[3])
                mRes.[2] <- (m1.[2] +*+ m2.[0]) ++ (m1.[3] +*+ m2.[2])
                mRes.[3] <- (m1.[2] +*+ m2.[1]) ++ (m1.[3] +*+ m2.[3])
                mRes
            if k = 2 
            then sqrMatrix mx mx 
            elif k % 2 = 1 
            then sqrMatrix (fibMatrix (k - 1)) mx
            else 
                let mRes = fibMatrix (k / 2)
                sqrMatrix mRes mRes
        let finalMx = fibMatrix n
        finalMx.[1]             

let fibArray n =  
    let arr = Array.create n (Num [0])        
    arr.[0] <- Num [1]
    arr.[1] <- Num [1]
    let i = 2
    for i in 2..n - 1  do
        arr.[i] <- arr.[i - 2] ++ arr.[i - 1]
    arr            