let mainFilter (list1:List<int>) (array1:array<int>) =
    let rec searchEqual i x =
            if  (i < array1.Length) && (x <> array1.[i]) 
            then
                if i = array1.Length - 1
                then true
                else searchEqual (i + 1) x
            else false
    let rec filter list = 
        if array1 = [||]
        then list
        else
            match list with
            | hd :: tl ->  
                if searchEqual 0 hd
                    then hd :: filter tl 
                    else filter tl 
            | [] -> []     
    filter list1
printfn "1. %A" (mainFilter [1; 3; 1; 0; 4; 5; 9] [|1; 3|])
printfn "2. %A" (mainFilter [1; 3; 1; 0; 4; 5; 9] [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|])
printfn "3. %A" (mainFilter [1; 3; 1; 0; 4; 5; 9] [||])
