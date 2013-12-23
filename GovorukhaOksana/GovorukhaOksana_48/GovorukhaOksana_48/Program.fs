
open Trees
open TTT
open newHash
open typeList

let func (newtr: TwoTypedThree) =
    let rec pl (lst: myList) (lst2: myList) = 
        match lst with
        | Lst (int, Empty) -> Lst (int, lst2)
        | Empty -> lst2
        | Lst (int, myList) ->  Lst (int, pl myList lst2)
        
    let rec f (newtr: TwoTypedThree) = 
        match newtr with
        | Dnode (tree, t, t2) -> pl (pl (main tree) (f t)) (f t2) 
        | Tnode (tree, t, t2, t3) -> pl (pl (pl (main tree) (f t)) (f t2)) (f t3)
        | Nleaf (tree) -> main tree
    
        
    f newtr

func (Tnode (Node("au", [Leaf "rdooge "]), Dnode (Leaf "rdooge ", Nleaf(Node("u", [Leaf "e"])), Nleaf(Node("a", [Leaf "r"]))), Nleaf (Leaf ("")), Nleaf (Leaf ("")))) |> printfn "%A"