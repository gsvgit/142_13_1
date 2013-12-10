module treeFunction

type Tree = 
    | Node of string * list<Tree>
    | Leaf of string

let f x = 
    let count = ref 0
    let rec main t = 
        match t with
        | Node (s, list) -> 
            if String.length s = 8
            then 
                count := !count + 1
                for i in 0..List.length list - 1 do main list.[i]
            else for i in 0..List.length list - 1 do main list.[i]
        | Leaf s -> 
            if String.length s = 8
            then count := !count + 1 
            else count := !count
    main x
    !count