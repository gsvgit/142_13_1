

type Long =
        | Num of list<int>
        static member (++) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail (addition.main (1::l1) (1::l2)))
        static member (--) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail (addition.main (1::l1) (-1::l2)))

let main x =
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
printfn "res = %A" (main 9)
