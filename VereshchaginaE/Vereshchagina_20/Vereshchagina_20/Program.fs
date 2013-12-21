let mainFilter (lst1 : List<int>) (arr1 : array<int>) =
    let rec search i x =
        if  (x <> arr1.[i]) && (i < arr1.Length) 
        then
            if i = arr1.Length - 1
            then false
            else search (i + 1) x
        else true
    
    let rec filter list = 
        if arr1 = [||]
        then list
        else
            match list with
            | hd :: tl ->  
                if search 0 hd
                    then filter tl 
                    else hd :: filter tl 
            | [] -> []     
    
    filter lst1

printfn "%A" (mainFilter [] [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|])
printfn "%A" (mainFilter [1; 2; 3; 4; 5; 6; 7; 8; 9] [||])
printfn "%A" (mainFilter [1; 3; 1; 2; 1; 1; 9] [|0; 2|])