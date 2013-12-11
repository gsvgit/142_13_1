open treeFunction

type List = 
    | Lst of Tree * List
    | Empty
let rec main x = 
    match x with
    | [] -> []
    | h::t -> f h::main t
printfn "%A" (main ([Node ("1", [Node ("lalalala", [Leaf "AAA"; Leaf "tratatat"; Leaf "TravinSoft"])]); Leaf "12345678"]))