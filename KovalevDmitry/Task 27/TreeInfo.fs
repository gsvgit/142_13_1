type Tree =
    | Node of string * list<Tree>
    | Leaf of string

let treeInfo (tr: Tree) =   
  
   let max = ref 0
   
   let rec strMax tr =   
       match tr with   
       | Node (str, lst) ->
           if str.Length > !max
           then 
               max := str.Length
               for i in 0..lst.Length - 1 do strMax lst.[i] 
           else
               for i in 0..lst.Length - 1 do strMax lst.[i]  
       | Leaf (str) ->
           if str.Length > !max
           then max := str.Length 
           else max := !max   
   
   strMax tr
   !max

printfn "The maximum length of the string in this tree = %A" (treeInfo (Node("Dead", [Node ("Leaves", [Node ("And", [Leaf ("The Dirty Ground") ] )] )] )))