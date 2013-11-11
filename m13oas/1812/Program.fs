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
printfn "res = %A" (main 2)
