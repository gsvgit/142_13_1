let mainFilter (lst:List<int>) (arr:array<int>) =

    let rec searchEq i x =
            if  (i < arr.Length) && (x <> arr.[i]) 
            then
                if i = arr.Length - 1
                then true
                else searchEq (i + 1) x
            else false
            
    let rec filt list = 
        if arr = [||]
        then list
        else
            match list with
            | [] -> []  
            | hd :: tl ->  
                if searchEq 0 hd
                    then hd :: filt tl 
                    else filt tl 
    filt lst
               
