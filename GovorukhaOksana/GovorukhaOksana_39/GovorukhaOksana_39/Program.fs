
open typeList
open Addition
open Multiplication

let myListTail lst = 
    match lst with 
    | Lst (int, myList) -> myList 
    | Empty -> Empty

type Long =
    | Num of myList
    static member (++) (lst, lst2) =
        match lst, lst2 with
        | Num lst, Num lst2 -> Num (Addition.main (Lst (1, lst)) (Lst (1, lst2)) |> myListTail) 
    static member (+*) (lst, lst2) =
        match lst, lst2 with
        | Num lst, Num lst2 -> Num (Multiplication.main (Lst (1, lst)) (Lst (1, lst2)) |> myListTail)  

let matr x =
    let n = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    let n1 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    let n2 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
    if x < 1 
    then
        failwith "error.incorrect input"
    elif x = 1 || x = 2
    then Num (Lst (1, Empty))
    else
        for i in 1 .. x - 1 do
            n.[0] <- (n1.[0] +* n2.[0]) ++ (n1.[1] +* n2.[2])
            n.[1] <- (n1.[0] +* n2.[1]) ++ (n1.[1] +* n2.[3])
            n.[2] <- (n1.[2] +* n2.[0]) ++ (n1.[3] +* n2.[2])
            n.[3] <- (n1.[2] +* n2.[1]) ++ (n1.[3] +* n2.[3])
            n1.[0] <- n.[0]
            n1.[1] <- n.[1]
            n1.[2] <- n.[2]
            n1.[3] <- n.[3]
        n.[2]
        
matr 11 |> printfn "89 = %A"
matr 3 |> printfn "%A"
matr 4 |> printfn "%A"
matr 5 |> printfn "%A"
matr 6 |> printfn "%A"
matr 7 |> printfn "%A"




 