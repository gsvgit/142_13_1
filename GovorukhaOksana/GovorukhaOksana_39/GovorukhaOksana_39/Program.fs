
open typeList
open Addition
open Multiplication

let t lst = 
    match lst with 
    | Lst (int, myList) -> myList 
    | Empty -> Empty
type Long =
    | Num of myList
    static member (++) (lst, lst2) =
        match lst,lst with
        | Num list, Num list2 -> Num (Addition.main (Lst (1, list)) (Lst (1, list2)) |> t) 
    static member (+*) (lst, lst2) =
        match lst,lst2 with
        | Num list, Num list2 -> Num (Multiplication.main (Lst (1, list)) (Lst (1, list2)) |> t)     

let matr x =
    let n = [|Num (Lst (0, Empty)); Num (Lst (0, Empty)); Num (Lst (0, Empty)); Num (Lst (0, Empty))|]
    let n2 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    let n3 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    if x <= 0 
    then
        failwith "error.incorrect input"
    else
        for i in 1 .. x - 1 do
            n.[0] <- (n2.[0] +* n3.[0]) ++ (n2.[1] +* n3.[2])
            n.[1] <- (n2.[0] +* n3.[1]) ++ (n2.[1] +* n3.[3])
            n.[2] <- (n2.[2] +* n3.[0]) ++ (n2.[3] +* n3.[2])
            n.[3] <- (n2.[2] +* n3.[1]) ++ (n2.[3] +* n3.[3])
            n2.[0] <- n.[0]
            n2.[1] <- n.[1]
            n2.[2] <- n.[2]
            n2.[3] <- n.[3]
        n.[1] 
        

matr 6 |> printfn "%A" 
 