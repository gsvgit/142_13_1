type Long =
        | Num of list<int>
        static member (++) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(addition.main (1::l1) (1::l2)))
        static member (--) ((l1:Long), (l2:Long)) =
            match l1, l2 with
            | Num l1, Num l2 -> Num (List.tail(addition.main (1::l1) (-1::l2)))

let main n =
    if n > 0 
    then
     let rec fib n1 n2 i = 
         if i = n
         then n2
         else fib (n1 ++ n2) n1 (i + 1)
     fib (Num [1]) (Num [1]) 1
    else failwith "Error. Please enter the correct index."
                
printfn "res = %A" (main 7)
printfn "res = %A" (main 1)
printfn "res = %A" (main 23)
