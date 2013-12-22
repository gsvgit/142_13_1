open myType
open Add
open Multip

let findingTail lst = 
    match lst with
    | Lst (h, t) -> t
    | Empty -> Empty

type Long =
    | Num of List
    static member (++) ((l1:Long), (l2:Long)) =
        match l1,l2 with
        | Num lst1, Num lst2 -> Num (Add.main (Lst (1, lst1)) (Lst (1, lst2)) |> findingTail)  
    static member (+*) ((l1:Long), (l2:Long)) =
        match l1,l2 with
        | Num lst1, Num lst2 -> Num (Multip.main (Lst (1, lst1)) (Lst (1, lst2)) |> findingTail) 

let main n = 
    let fibN = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    let fibN1 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    let fibN2 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
  
    if n > 0 
    then
        for i in 1..n - 1 do
            fibN.[0] <- (fibN1.[0] +* fibN2.[0]) ++ (fibN1.[1] +* fibN2.[2])
            fibN.[1] <- (fibN1.[0] +* fibN2.[1]) ++ (fibN1.[1] +* fibN2.[3])
            fibN.[2] <- (fibN1.[2] +* fibN2.[0]) ++ (fibN1.[3] +* fibN2.[2])
            fibN.[3] <- (fibN1.[2] +* fibN2.[1]) ++ (fibN1.[3] +* fibN2.[3])
            fibN1.[0] <- fibN.[0]
            fibN1.[1] <- fibN.[1]
            fibN1.[2] <- fibN.[2]
            fibN1.[3] <- fibN.[3]
        fibN.[2]
    else 
        failwith "Error! Wrong index!"

main 12 |> printfn "%A"
