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
    printfn "There are %A strings which have 8 symbols" !count
printfn "%A" (f (Node ("12345679", [Node ("8", [Node ("sdqwerty", [Leaf ("331 cows"); Leaf ("33 cows"); Leaf ("Wendigoo")]); Leaf ("1")]); Leaf ("HelloWor")])))