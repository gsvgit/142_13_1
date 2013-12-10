open treeFunction

type listOfTree =
    | Lst of Tree * listOfTree
    | Empty

let rec main (list:listOfTree) =
    match list with
    | Lst (tree, listOfTree) -> treeFunction.main tree :: main listOfTree        
    | Empty -> []

let myList = Lst (Node ("abc", [Node ("def", [Leaf "hij"; Leaf "klm"; Leaf "nop"]);Node ("qrs", [Leaf "tuv"; Leaf "wm"; Leaf "yz"])]),
                Lst (Node ("bc", [Node ("def", [Leaf "klm"; Leaf "nop"]);Node ("qrs", [Leaf "wm"; Leaf "yz"])]),
                    Lst (Node ("bc", [Node ("df", [Leaf "klm"; Leaf "np"]);Node ("qrs", [Leaf "tu"; Leaf "wm"; Leaf "y"])]), Empty)))

printfn "%A" (main myList)
