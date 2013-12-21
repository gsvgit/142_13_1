module Tree

type Tree = 
    | Node of string * list<Tree>
    | Leaf of string
 
let func x = 
    let a = ref 0
    let rec main t = 
        match t with
        | Node (s, list) -> 
            for i in 0..List.length list - 1 do main list.[i]
            a := !a
            
        | Leaf s ->  
            a := !a + 1
            
    main x
    printfn "We have %A Nodes" !a

func (Node ("in", 
        [Node ("game", 
            [Node ("of", 
                [Leaf ("thrones"); 
                 Leaf ("you"); 
                 Leaf ("win")]); 
                 Leaf ("or")]); 
                 Leaf ("you"); 
                 Leaf ("die")]))