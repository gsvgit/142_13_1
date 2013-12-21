module Trees

type Tree =
| Node of string * list<Tree>
| Leaf of string

let maxLeaves tr = 

    let check tr = 
        match tr with
        | Node (str, trr) -> true
        | Leaf (str) -> false

    if check tr 
    then
        let l = ref 0
        let k = ref 0
    
        let rec countNode tr =
                match tr with 
                | Node (str, lst) ->                
                    k := !k + 1 
                    for i in 0 .. lst.Length - 1 do countNode lst.[i]
                | _ -> ()

        countNode tr 
        let array = Array.zeroCreate !k
        let i = ref 0
    
        let rec countLeaf (lst: List<Tree>)  =
            if !i < lst.Length 
            then
                match lst.[!i] with
                | Node (str, lst1) -> 
                        i := !i + 1
                        countLeaf lst 
                | Leaf (str) -> 
                        l := !l + 1 
                        i := !i + 1  
                        countLeaf lst  
        let n = ref -1
    
        let rec f tr =
            match tr with 
            | Node (str, lst) -> 
                    l := 0
                    i := 0
                    n := !n + 1
                    countLeaf lst
                    array.[!n] <- !l
                    for j in 0 .. lst.Length - 1 do f lst.[j]
            | _ -> ()

        f tr
        Array.max array 

    else 1

maxLeaves (Node ( "df", [Node ("gdg", [Leaf (""); Node ("dd", [Leaf ("")])]); Leaf (""); Leaf ("")])) |> printfn "%A"
maxLeaves (Node ( "df", [Node ("gdg", [Leaf (""); Leaf (""); Leaf (""); Node ("dd", [Leaf ("")])]); Leaf (""); Leaf ("")])) |> printfn "%A"
maxLeaves (Leaf ("")) |> printfn "%A" 
maxLeaves (Node ("h", [Leaf ("")])) |> printfn "%A" 

    