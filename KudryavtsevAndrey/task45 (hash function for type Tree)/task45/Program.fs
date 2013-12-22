open hash
open TreeFunction 

let hashTree (tree: Tree) = 
    let a = ref 0 
    let rec main tr = 
        match tr with 
        | Node (str, list) -> 
            a := !a + hashString(str)
            for i in 0..list.Length - 1 do main list.[i] 
        | Leaf str -> 
            a := !a + hashString(str)
    main tree
    printfn "res = %A" !a 

hashTree (Node ("12", [Node ("23", [Node ("3", [Leaf ("4"); Leaf ("5")]); Leaf ("6")])]))

