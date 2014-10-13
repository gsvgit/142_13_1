namespace Problem

module Main =
    let main1 inputArray num =
        inputArray
        |> Array.mapi (fun index elem -> if elem > num then Some (index) else None)
        |> Array.filter (fun elem -> elem.IsSome)
        |> Array.map (fun elem -> elem.Value)

    let main2 inputArray num =
        let len = Array.length inputArray
        let rec count i acc =
            if i >= len
            then acc
            elif inputArray.[i] > num
            then count (i + 1) (acc + 1)
            else count (i + 1) acc
        let indexArray = count 0 0 |> Array.zeroCreate
        let rec collectIndexes pos1 pos2 =
            if pos1 >= len
            then indexArray
            elif inputArray.[pos1] > num
            then
                indexArray.[pos2] <- pos1
                collectIndexes (pos1 + 1) (pos2 + 1)
            else collectIndexes (pos1 + 1) pos2
        collectIndexes 0 0

    let main a b =
        let x = main1 a b
        let y = main2 a b
        if x = y
        then x
        else failwith "One or more implementations work incorrect!"