type Long =
        | Num of list<int>
        static member (++) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(addition.main (1::l1) (1::l2)))
        static member (--) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(addition.main (1::l1) (-1::l2)))
        static member (+*+) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(multiplication.main (1::l1) (1::l2)))


let main x =
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
printfn "res = %A" (main 480)
