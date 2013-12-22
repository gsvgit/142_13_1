module public myType
 
type MyList = 
    | List of int * MyList
    | Empty

let myListTail (lst: MyList) = 
        match lst with 
        | List (hd, tl) -> tl 
        | Empty -> Empty

let myListHead (lst: MyList) = 
        match lst with
        | List (hd, tl) -> hd
        | Empty -> 0 
 
let myList = List (1, List (2, List (3, List (4, List (5, Empty)))))

printfn "%A" myList 
