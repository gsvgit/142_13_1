open TreeFunction

type listOfTree =
    | Lst of Tree * listOfTree
    | Empty

let rec main (list: listOfTree) = 
    match list with 
    | Lst (tree, lst) -> treeInfo tree :: main lst
    | Empty -> []

main (Lst (Node ("1", [Node ("2", [Leaf "3"; Leaf "4"]) ]), Empty)) |> printfn "%A"
