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
     
printfn "res = %A" (main 9)
