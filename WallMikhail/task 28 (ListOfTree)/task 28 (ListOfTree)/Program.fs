open Tree
open MyT

type ListOfTree =
    | Lst of Tree.Tree * ListOfTree
    | Empty

let main lst =
    let rec f lst =
        match lst with 
        | hd :: tl -> Tree.func hd :: f tl
        | [] -> []
    f lst

printfn "%A" (main [Node ("This", [Node ("is", [Leaf "your"; Leaf "life"])])]) 
printfn "%A" (main [Node ("Stop" , [Node ("waiting", [Node ("things", [Leaf "to"; Leaf "happen"])])])])