namespace Problem

module Main =
    let stringHash str =
        let l = String.length str
        let rec sum i acc =
            if i < l
            then sum (i + 1) (acc + int str.[i])
            else acc
        sum 0 0

    let rec hashAll lst =
        match lst with
        | h :: t -> stringHash h :: hashAll t
        | [] -> []