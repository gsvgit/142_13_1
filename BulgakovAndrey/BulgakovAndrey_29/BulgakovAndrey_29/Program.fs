open TreeListTree
type MyList =
    | Lst of Tree * MyList
    | Empty
let SomethingHard tre =
    let rec f tr =
        match tr with
        | h :: d -> TreeListTree.inf h :: f d
        | [] -> []
    f tre
printfn "%A" <|
(SomethingHard 
[
Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block1"])]); Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block2"]) ]) ]) ]);
Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block1"])]); Node ("bl", [Node ("bb", [Node ("bl", [Leaf "block2"]) ]) ]) ])
]
)