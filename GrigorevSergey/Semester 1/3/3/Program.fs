namespace Problem

module Main =
    let main1 inputArray num =
        inputArray
        |> Array.mapi (fun index elem -> if elem > num then index else -1)
        |> Array.filter (fun elem -> if elem < 0 then false else true)

    let main2 (inputArray: array<int>) num =
        let rec count i acc =
            if i >= inputArray.Length
            then acc
            elif inputArray.[i] > num
            then count (i + 1) (acc + 1)
            else count (i + 1) acc
        let indexArray = count 0 0 |> Array.zeroCreate
        let rec collectIndexes pos1 pos2 =
            if pos1 >= inputArray.Length
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