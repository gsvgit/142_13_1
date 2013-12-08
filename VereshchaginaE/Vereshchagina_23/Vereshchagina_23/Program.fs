module public myType
 
type MyList = 
    | List of int * MyList
    | Empty
 
let myList = List (1, List (2, List (3, List (4, List (5, Empty)))))

printfn "%A" myList 
