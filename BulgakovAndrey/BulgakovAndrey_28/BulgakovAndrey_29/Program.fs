open TreeListTree
type MyList =
    | Lst of Tree * MyList
    | Empty
let main tre =
    let rec f tr =
        match tr with
        | h :: d -> TreeListTree.inf h :: f d
        | [] -> []
    f tre
printfn "%A" <|
(main 
[
Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block1"])]); Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block2"]) ]) ]) ]);
Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block1"])]); Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block2"]) ]) ]) ])
]
)