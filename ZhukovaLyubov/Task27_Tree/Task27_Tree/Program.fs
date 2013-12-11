module tree
type Tree =
    | Node of string * list<Tree>
    | Leaf of string

let info (x: Tree) =   
   let count = ref 0   
   let rec odd x =   
       match x with   
       | Node (string, list) ->
           for i in 0 .. string.Length - 1 do
           if string.Length % 2 <> 0
           then count := !count + 1 
           else count := !count 
       | Leaf (string) ->
           for i in 0 .. string.Length - 1 do
           if string.Length % 2 <> 0
           then count := !count + 1 
           else count := !count 
   odd x
   !count 
printfn "%A" (info (Node ("abcde", [Leaf "fgh"; Leaf "ij"; Node ("klmno", [Leaf "pq"; Leaf "rstuvwxyz"])])))
printfn "%A" (info (Leaf ""))


   