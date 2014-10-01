namespace Problem

module Main =
    type Tree =
    | Node of string * list<Tree>
    | Leaf of string

    let rec bypass func state tree =
        match tree with
        | Leaf (a) -> func state a
        | Node (a, t) ->
            let state = func state a
            let state = List.fold (fun st el -> bypass func st el) state t
            state

    let sumOfLengths tree = bypass (fun state elem -> state + String.length elem) 0 tree