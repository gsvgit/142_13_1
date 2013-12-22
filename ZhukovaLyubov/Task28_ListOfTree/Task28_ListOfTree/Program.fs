open tree
type List =
    | Lst of Tree * List
    | Empty

let rec inf treeList =
    match treeList with
    | Lst (tree, lstTree) -> info tree :: inf lstTree
    | Empty -> []

printfn "%A" (inf (Lst (Node ("abcdeg", [Node ("fgh", [Leaf "ij"])]), Lst (Node ("klmno", [Node ("pq", [Leaf "rstuvwxyz"])]), Empty))))
