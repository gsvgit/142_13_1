
let mainFilter (lst: List<int>) (arr: array<int>) = 
    if arr = [||] 
    then lst
    else    
        let rec main hd (arr: array<int>) i =
            if hd <> arr.[i] 
            then 
                if i = arr.Length - 1
                then true
                else main hd arr (i + 1) 
            else false
     
        let rec main2 (lst: List<int>) (arr: array<int>) =            
            match lst with
            | hd :: tl -> 
                        if main hd arr 0 
                        then hd :: main2 tl arr
                        else main2 tl arr
            | [] -> []
 
        main2 lst arr
            
mainFilter [1; 3; 1; 2; 1; 2; 6] [|1; 3; 5; 7; 9|]  |> printfn "%A"    
mainFilter [0; 2; 6; 9] [||] |> printfn "%A"              
                     