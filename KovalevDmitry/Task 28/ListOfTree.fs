open TreeInfo

type MyTreeList =
| Lst of Tree * MyTreeList
| Empty

let rec checkThemAll (trLst: MyTreeList) =
    match trLst with
    | Lst (tree, tLst) -> treeInfo tree :: checkThemAll tLst
    | Empty -> []
    
checkThemAll (Lst (Node ("666666", [Node ("333", [Leaf "1"]) ]), Lst (Node ("55555", [Leaf "22"] ), Empty))) |> printfn "%A"     