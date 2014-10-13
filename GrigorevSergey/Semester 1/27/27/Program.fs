namespace Problem

module Main =
    type Tree =
    | Node of string * list<Tree>
    | Leaf of string

    let rec iterate func state tree =
        match tree with
        | Leaf (a) -> func state a
        | Node (a, t) ->
            let state = func state a
            let state = List.fold (fun st el -> iterate func st el) state t
            state

    let sumOfLengths tree = iterate (fun state elem -> state + String.length elem) 0 tree