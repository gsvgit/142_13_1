open Tree

type ListOfTree =
    | Lst of Tree.Tree * ListOfTree
    | Empty

let rec func (list:ListOfTree) = 
    match list with 
    | Lst (tree, lst) -> treeHash tree :: func lst
    | Empty -> []


printfn "%A" (func (Lst (Node ("ghbdtn", [Node ("pkjq", [Leaf "rjdfhysq"]) ]), Empty)))
printfn "%A" (func (Empty))



