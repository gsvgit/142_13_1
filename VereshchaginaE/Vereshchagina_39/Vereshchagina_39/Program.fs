open myType

open addition

open multiplication

type Long =
    |Num of MyList
    static member (++) (l1, l2) =
        match l1,l2 with
        | Num l1, Num l2 -> Num ((addition.main (List (1, l1)) (List (1, l2))) |> myType.myListTail)
    static member (+*) (l1, l2) =
        match l1,l2 with
        | Num l1, Num l2 -> Num ((multiplication.main (List (1, l1)) (List (1, l2))) |> myType.myListTail)
           

let main n =
    if n < 1
    then failwith "Incorrect index"
    else
        if n = 1
        then Num (List (1, Empty))
        else        
            let fibN = [|Num (List (0, Empty)); Num (List (0, Empty)); Num (List (0, Empty)); Num (List (0, Empty))|]
            let fibN1 = [|Num (List (1, Empty)); Num (List (1, Empty)); Num (List (1, Empty)); Num (List (0, Empty))|]
            let fibN2 = [|Num (List (1, Empty)); Num (List (1, Empty)); Num (List (1, Empty)); Num (List (0, Empty))|]
            for i in 1..n - 1 do
                fibN.[0] <- (fibN1.[0] +* fibN2.[0]) ++ (fibN1.[1] +* fibN2.[2])
                fibN.[1] <- (fibN1.[0] +* fibN2.[1]) ++ (fibN1.[1] +* fibN2.[3])
                fibN.[2] <- (fibN1.[2] +* fibN2.[0]) ++ (fibN1.[3] +* fibN2.[2])
                fibN.[3] <- (fibN1.[2] +* fibN2.[1]) ++ (fibN1.[3] +* fibN2.[3])
                fibN1.[0] <- fibN.[0] 
                fibN1.[1] <- fibN.[1] 
                fibN1.[2] <- fibN.[2]
                fibN1.[3] <- fibN.[3]
            fibN.[1]  

printfn "FIb(13) %A" (main 13)
printfn "%A" (main 0)
