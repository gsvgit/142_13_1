open myType

type myList =
| Lst of int*myList
| Empty

let rec lstLength (lst, myList) =
    match lst with 
    | lst (int, MyList) -> lstLength myList + 1
    | Empty -> 0 

