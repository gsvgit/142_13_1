module typeList

type MyList =
    | Lst of int * MyList
    | Empty

let rec length list =
    match list with 
    | Lst (hd, myList) -> length myList + 1
    | Empty -> 0

let head list =
    match list with 
    | Lst (hd, myList) -> hd
    | Empty -> 0

let tail list =
    match list with 
    | Lst (hd, myList) -> myList
    | Empty -> Empty

let rec rev list =
    
    let rec Last list =
        match list with
        | Empty -> 0
        | Lst (hd, Empty) -> hd
        | Lst (hd, myList) -> Last myList
        
    let rec delete list =
        match list with 
        | Empty -> Empty
        | Lst (hd, Empty) -> Empty
        | Lst (hd, myList) -> Lst (hd, delete myList)       
    
    match list with
    | Empty -> Empty
    | Lst (hd, myList) -> Lst (Last list, delete list |> rev)