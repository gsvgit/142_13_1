open MyTypeTree

type ListTree =
| Lst of MyTypeTree.Tree * ListTree
| Empty

let main lsttr =
    let rec listinf lsttr =
        match lsttr with
        | hd :: tl -> MyTypeTree.main hd :: listinf tl 
        | [] -> []
    listinf lsttr

[
Node ("a", [Node ("aryt", [Node ("yrew", [Leaf "article"]) ]); Node ("apr", [Node ("rew", [Leaf "afghrr"]) ]) ]); 
Node ("ba", [Node (".har", [Node ("rew", [Leaf "ardcr"]) ]); Node ("aras", [Node ("rew", [Leaf "afgnlrr"]) ]) ])
] |> printfn "%A" 

(main 
[
Node ("a", [Node ("aryt", [Node ("yrew", [Leaf "article"]) ]); Node ("apr", [Node ("rew", [Leaf "afghrr"]) ]) ]); 
Node ("ba", [Node (".har", [Node ("rew", [Leaf "ardcr"]) ]); Node ("aras", [Node ("rew", [Leaf "afgnlrr"]) ]) ])
]
) |> printfn "%A" 