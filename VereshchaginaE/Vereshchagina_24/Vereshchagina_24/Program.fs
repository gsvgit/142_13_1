open myType

let length (list : MyList) =
    let len = ref 0
    let rec count (lst : MyList) =
        match lst with
        | List(int, MyList) -> 
            len := !len + 1
            count MyList  
        | Empty -> len
    count list      
        
let myList = List (1, List (2, List (3, List (4, List (5,Empty)))))

printfn "%A" (length myList)
