module AccessoryFunctions 

open myType
open listLength

let myListHead (lst: myList) = 
    match lst with
    | Lst (hd, tl) -> hd
    | Empty -> 0 

let myListTail (lst: myList) = 
    match lst with 
    | Lst (hd, tl) -> tl 
    | Empty -> Empty

let rec myListReverse (lst: myList) =
        let rec returnLast lst =
            match lst with
            | Lst (hd, Empty) -> hd
            | Lst (hd, t) -> returnLast t
            | Empty -> 0
        let rec cut lst =
            match lst with 
            | Lst (hd, Empty) -> Empty
            | Lst (hd, tl) -> Lst (hd, cut tl)
            | Empty -> Empty
        match lst with
        | Empty -> Empty
        | Lst (hd, tl) -> Lst (returnLast lst, myListReverse (cut lst))