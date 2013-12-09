
type Tree = 
    | Node of string * list<Tree>
    | Leaf of string

let treeInfo (tree: Tree) = 
    let amount = ref 0 
    let rec main tr = 
        match tr with 
        | Node (str, list) -> 
            amount := !amount + str.Length 
            for i in 0..list.Length - 1 do main list.[i] 
        | Leaf str -> 
            amount := !amount + str.Length 
    main tree 
    printfn "Amount of symbols in tree: %A" !amount

treeInfo (Node ("I'm", 
         [Node ("not", 
            [Node ("afraid", 
                [Node ("to take", 
                    [Leaf ("a stand"); 
                     Leaf ("so everybody"); 
                     Leaf ("come")]); 
                     Leaf ("take")]); 
                     Leaf ("my"); 
                     Leaf ("hand")])]))