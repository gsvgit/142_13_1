module TreeListTree
type Tree =
    | Node of string * list<Tree>
    | Leaf of string
let hashfun (str:string) =
    if str.Equals("") = true
    then 0
    else int str.[0]
let inf tr =
    let rec infa tr1 t=
        let rec infa2 ls tk=
            match ls with
            | hd :: tl -> infa hd 0 + infa2 tl tk
            | [] -> tk 
        match tr1 with
        | Node (str, lst) -> infa2 lst (hashfun str)
        | Leaf (str) -> hashfun str
    infa tr 0
printfn "%A" (inf (Node ("bba", [Node ("br", [Node ("brd", [Leaf "bfr"]) ]); Node ("brf", [Node ("brf", [Leaf "brf"]) ]) ])))
printfn "%A" (inf (Leaf ""))