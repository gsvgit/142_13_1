﻿//open myType


type myList = 
    | Lst of int * myList
    | Empty

let rec lstLength lst = 
    match lst with 
    | Lst (int, myList) -> lstLength myList + 1
    | Empty -> 0

printfn "%A" (lstLength (Lst (124, Lst (24, Empty))))
printfn "%A" (lstLength (Empty))