type MyList =
| Lst of int*MyList
| Empty

let lst = Lst(1, Lst(2, Empty))

lst |> printfn "%A" 

type Tree =
| Node of int*Tree*Tree*Tree 
| Leaf of int

let leafs = Leaf(6), 1
let nd = Node(2013,
            Node(1, Leaf(1), Leaf(2), Leaf(3)), 
            Node(1, Leaf(4), Leaf(0), Leaf(10)),
            Leaf(3))
nd |> printfn "%A"