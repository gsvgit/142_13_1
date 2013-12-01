type Tree =
    | Node of string * list<Tree>
    | Leaf of string

let main tree = 
    let rec num tr =
        let rec number lst =
            match lst with
            | hd :: tl -> num hd + number tl
            | [] -> 0        
        match tr with
        | Node (s, tre) ->
            if s.[0] = 'a'
            then 1 + number tre
            else number tre
        | Leaf k ->
            if k.[0] = 'a'
            then 1
            else 0 
    num tree

printfn "%A" (main (Node ("aba", [Node ("ar", [Node ("rew", [Leaf "arr"]) ]); Node ("ar", [Node ("rew", [Leaf "arr"]) ]) ])) )