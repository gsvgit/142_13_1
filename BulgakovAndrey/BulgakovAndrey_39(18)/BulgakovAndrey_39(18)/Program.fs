open MAddition
open MMulti
open TypeList

type Long =
    | Num of MyList
    static member (++) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num l1, Num l2 -> Num (MAddition.main l1 l2)
    static member (--) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num l1, Num l2 -> Num (MAddition.main l1 (Lst(((MAddition.head l2) * -1), (MMulti.tail l2))))
    static member (-*-) ((l1:Long), (l2:Long)) = 
        match l1, l2 with
        | Num l1, Num l2 -> Num (MMulti.tail (MMulti.main (Lst(1, l1)) (Lst(1, l2))))

let mat n =
  if n < 1
  then failwith "Error. Please enter the correct index."
  else
    let arr = [|Num (Lst(1, Empty)); Num (Lst(1, Empty)); Num (Lst(1, Empty)); Num (Lst(0, Empty))|]
    let arr1 = [|Num (Lst(1, Empty)); Num (Lst(1, Empty)); Num (Lst(1, Empty)); Num (Lst(0, Empty))|]
    let arr2 = [|Num (Lst(1, Empty)); Num (Lst(1, Empty)); Num (Lst(1, Empty)); Num (Lst(0, Empty))|]
    for i in 1..n do
        arr1.[0] <- (arr.[0] -*- arr2.[0]) ++ (arr.[1] -*- arr2.[2])
        arr1.[1] <- (arr.[0] -*- arr2.[1]) ++ (arr.[1] -*- arr2.[3])
        arr1.[2] <- (arr.[2] -*- arr2.[0]) ++ (arr.[3] -*- arr2.[2])
        arr1.[3] <- (arr.[2] -*- arr2.[1]) ++ (arr.[3] -*- arr2.[3])
        arr.[0] <- arr1.[0]
        arr.[1] <- arr1.[1]
        arr.[2] <- arr1.[2]
        arr.[3] <- arr1.[3]   
    arr.[3]

printfn "%A" (mat 100)