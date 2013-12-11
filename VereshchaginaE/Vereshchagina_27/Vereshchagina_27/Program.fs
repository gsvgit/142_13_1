module Tree

open Hash

type Tree = 
    | Node of string * list<Tree>
    | Leaf of string
 
let treeHash (tree:Tree) = 
    let a = ref 0
    let b = ref 0
    let rec treeFunc (tr:Tree) = 
        match tr with
        | Node (str, list) -> 
            a := !a + Hash.stringHash(str)
            b := !b + 1
            for i in 0..List.length list - 1 do treeFunc list.[i]
        | Leaf str ->  
            a := !a + Hash.stringHash(str)
            b := !b + 1
    treeFunc tree
    let a = !a
    let b = !b
    a % b
    
printfn "%A" (treeHash (Node ("yjcjr", [Leaf "yf"; Leaf "dtifkrt"; Node ("f[f[f[f", [Leaf "z"; Leaf "egjhjkfcm"])])))
                      
                      
                     
