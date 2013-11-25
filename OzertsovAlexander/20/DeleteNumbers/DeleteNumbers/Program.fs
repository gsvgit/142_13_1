let main (arr:array<_>) (num:list<_>) =
    
    let rec common i k (arr2:array<_>) =
        if (i <= arr2.Length - 1) && k <> arr2.[i]
        then 
            if i = arr2.Length - 1
            then true
            else (common (i+1) k arr2)
        else false    
    
    let  rec body (arr1:array<_>) num = 
        if arr1 = [||]
        then
            num
        else
            match num with
            | hd :: tl ->
                if common 0 hd arr1
                then hd :: body arr tl
                else body arr tl
            | [] -> []
    body arr num

printfn "res = %A" (main [|3; 2; 0; 5; 5|] [1; 3; 1; 0; 4; 3; 5])