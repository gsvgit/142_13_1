open Trees

type ListOfTree =
| Lst of Tree * ListOfTree
| Empty

let main (lstree: ListOfTree) =
    let rec f lstree =
        match lstree with
        | Lst (tree, lsttr) -> Trees.maxLeaves tree :: f lsttr
        | Empty -> []

    f lstree

main (Lst (Node ("gdg", [Leaf (""); Leaf (""); Leaf (""); Node ("dd", [Leaf ("")])]), 
           Lst (Node ("gdg", [Leaf(""); Node ("dd", [Leaf (""); Leaf ("")])]), Empty))) |> printfn "%A"

main (Lst (Leaf (""), Empty)) |> printfn "%A"
main (Lst (Node ("h", [Leaf ("")]), Empty)) |> printfn "%A"
main (Lst (Leaf (""), Lst (Leaf(""), Empty))) |> printfn "%A"
    