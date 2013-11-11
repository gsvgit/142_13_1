type Long =
        | Num of list<int>
        static member (++) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(addition.main (1::l1) (1::l2)))
                
let main x =
   let rec fibXXX n =
        if n < 1 
        then failwith "error index"
        elif n = 1 || n = 2
        then Num [1]
        else fibXXX (n - 1) ++ fibXXX (n - 2)
   fibXXX x
printfn "res = %A" (main 5)
printfn "res = %A" (main 10)