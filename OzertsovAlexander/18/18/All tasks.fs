module Task
type Long =
        | Num of list<int>
        static member (++) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(Addition.main (1::l1) (1::l2)))
        static member (--) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(Addition.main (1::l1) (-1::l2)))
        static member (+*+) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(Multiplication.main (1::l1) (1::l2)))

let rec recurs n =
        if n < 1 
        then failwith "error index"
        elif n = 1 || n = 2
        then Num [1]
        else recurs (n - 1) ++ recurs (n - 2)


let iteration x =
    if x < 1
    then
        failwith "error index"
    else
        let f1 = ref (Num [1])
        let f2 = ref (Num [1])
        for i in 2..x do
            f2 := !f2 ++ !f1
            f1 := !f2 -- !f1
        !f1

let iterarionwithoutref n =
    if n > 0 
    then
     let rec fib n1 n2 i = 
         if i = n
         then n2
         else fib (n1 ++ n2) n1 (i + 1)
     fib (Num [1]) (Num [1]) 1
    else failwith "Error. Please enter the correct index."

let matrix x =
    let mat = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let mat1 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
    let mat2 = [|Num [1]; Num [1]; Num [1]; Num [0]|]
  
    if x > 0 then
        for i in 1..x-1 do
            mat.[0] <- (mat1.[0] +*+ mat2.[0]) ++ (mat1.[1] +*+ mat2.[2])
            mat.[1] <- (mat1.[0] +*+ mat2.[1]) ++ (mat1.[1] +*+ mat2.[3])
            mat.[2] <- (mat1.[2] +*+ mat2.[0]) ++ (mat1.[3] +*+ mat2.[2])
            mat.[3] <- (mat1.[2] +*+ mat2.[1]) ++ (mat1.[3] +*+ mat2.[3])
            mat1.[0] <- mat.[0]
            mat1.[1] <- mat.[1]
            mat1.[2] <- mat.[2]
            mat1.[3] <- mat.[3]
        mat.[2]
     else failwith "error index"

let matrixlog x =
    if x > 0
    then 
        if x = 1
        then Num [1]
        else
            let rec fib n =
                let mat = [|Num [1]; Num [1]; Num [1]; Num[0]|]
                let multi (array1:array<_>) (array2:array<_>) =
                    let array = [|Num [0]; Num [0]; Num [0]; Num [0]|]
                    array.[0] <- (array1.[0] +*+ array2.[0]) ++ (array1.[1] +*+ array2.[2])
                    array.[1] <- (array1.[0] +*+ array2.[1]) ++ (array1.[1] +*+ array2.[3])
                    array.[2] <- (array1.[2] +*+ array2.[0]) ++ (array1.[3] +*+ array2.[2])
                    array.[3] <- (array1.[2] +*+ array2.[1]) ++ (array1.[3] +*+ array2.[3])
                    array
                if n = 2
                then multi mat mat
                elif n % 2 = 1
                    then multi (fib (n - 1)) mat
                    else 
                        let array = fib (n / 2)
                        multi array array
            let elem (array3:array<_>) = 
                array3.[2]
            elem (fib x)
    else failwith "error index"

let fibarray x =
    if x < 1
    then
        failwith "error index"
    elif x = 1
    then [|Num [1]|]
    else
        let arr = Array.zeroCreate x
        arr.[0] <- Num [1]
        arr.[1] <- Num [1]
        for i in 2..x - 1 do
                arr.[i] <- arr.[i - 1] ++ arr.[i - 2]
        arr