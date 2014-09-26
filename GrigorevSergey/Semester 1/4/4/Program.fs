namespace Problem

module Main =
    let main1 inputArray lowBound highBound =
        inputArray
        |> Array.mapi (fun index elem -> if elem > lowBound && elem < highBound then Some(index) else None)
        |> Array.filter (fun elem -> elem.IsSome)
        |> Array.map (fun elem -> elem.Value)

    let main2 (inputArray: array<int>) lowBound highBound =
        let rec count i acc =
            if i >= inputArray.Length
            then acc
            elif inputArray.[i] > lowBound && inputArray.[i] < highBound
            then count (i + 1) (acc + 1)
            else count (i + 1) acc
        let indexArray = count 0 0 |> Array.zeroCreate
        let rec collectIndexes pos1 pos2 =
            if pos1 >= inputArray.Length
            then indexArray
            elif inputArray.[pos1] > lowBound && inputArray.[pos1] < highBound
            then
                indexArray.[pos2] <- pos1
                collectIndexes (pos1 + 1) (pos2 + 1)
            else collectIndexes (pos1 + 1) pos2
        collectIndexes 0 0

    let main a b c =
        let x = main1 a b c
        let y = main2 a b c
        if x = y
        then x
        else failwith "One or more implementations work incorrect!"