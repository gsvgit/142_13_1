namespace Problem

module Main =
    type MyList<'T> =
        | ListNode of 'T * MyList<'T>
        | ListLeaf

    let rec myListFromList lst =
        match lst with
        | h :: t -> ListNode (h, myListFromList t)
        | [] -> ListLeaf

    let myListFromArray arr =
        let n = Array.length arr
        let rec construct i =
            if i < n
            then ListNode (arr.[i], construct (i + 1))
            else ListLeaf
        construct 0

    let rec myListLength mlst =
        match mlst with
        | ListLeaf -> 0
        | ListNode (_, t) -> 1 + myListLength t

    type Tree =
    | Node of string * list<Tree>
    | Leaf of string

    let rec iterateTree func state tree =
        match tree with
        | Leaf a -> func state a
        | Node (a, t) ->
            let state = func state a
            let state = List.fold (fun st el -> iterateTree func st el) state t
            state

    let rec iterateList func state lst =
        match lst with
        | ListNode (h, t) -> (iterateTree func state h) :: iterateList func state t
        | ListLeaf -> []

    let main lst = iterateList (fun state elem -> state + String.length elem) 0 lst