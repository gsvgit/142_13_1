﻿namespace Problem

module Main =
    type MyList<'T> =
        | Node of 'T * MyList<'T>
        | Leaf

    let rec myListFromList lst =
        match lst with
        | h :: t -> Node (h, myListFromList t)
        | [] -> Leaf

    let myListFromArray arr =
        let n = Array.length arr
        let rec construct i =
            if i < n
            then Node (arr.[i], construct (i + 1))
            else Leaf
        construct 0

    let rec myListLength mlst =
        match mlst with
        | Leaf -> 0
        | Node (_, t) -> 1 + myListLength t

    let main = myListLength