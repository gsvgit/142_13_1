open typeList

open addition

open multiplication

type Long =
    |Num of MyList
    static member (+*) ((l1:Long), (l2:Long)) =
        match l1,l2 with
        | Num l1, Num l2 -> Num ((multiplication.main (Lst (1, l1)) (Lst (1, l2))) |> tail)
    static member (++) ((l1:Long), (l2:Long)) =
        match l1,l2 with
        | Num l1, Num l2 -> Num ((addition.main (Lst (1, l1)) (Lst (1, l2))) |> tail)       

let main n =
    if n < 1
    then failwith "An invalid value FibN"
    else
        if n = 1
        then Num (Lst (1, Empty))
        else        

            let fibN = [|Num (Lst (0, Empty)); Num (Lst (0, Empty)); Num (Lst (0, Empty)); Num (Lst (0, Empty))|]
            let fibN1 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0, Empty))|]
            let fibN2 = [|Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (1, Empty)); Num (Lst (0,Empty))|]

            for i in 1 .. n - 1 do
                fibN.[0] <- (fibN1.[0] +* fibN2.[0]) ++ (fibN1.[1] +* fibN2.[2])
                fibN.[1] <- (fibN1.[0] +* fibN2.[1]) ++ (fibN1.[1] +* fibN2.[3])
                fibN.[2] <- (fibN1.[2] +* fibN2.[0]) ++ (fibN1.[3] +* fibN2.[2])
                fibN.[3] <- (fibN1.[2] +* fibN2.[1]) ++ (fibN1.[3] +* fibN2.[3])
                fibN1.[0] <- fibN.[0] 
                fibN1.[1] <- fibN.[1] 
                fibN1.[2] <- fibN.[2]
                fibN1.[3] <- fibN.[3]
            fibN.[1]  

printfn "1. %A" (main 25)
printfn "2. %A" (main 1)
printfn "3. %A" (main 0)