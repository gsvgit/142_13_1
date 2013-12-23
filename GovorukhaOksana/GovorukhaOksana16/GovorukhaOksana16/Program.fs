module Addition

let main (lst: List<int>) (lst2: List<int>) = 
    let rec check lst = 
        match lst with
        | hd :: tl -> 
             if (-10 < hd) && (hd < 10) 
             then check tl
             else false
        | [] -> true
    if (lst.Head = -1 || lst.Head = 1) && (lst2.Head = -1 || lst2.Head = 1) &&
        check lst && check lst2 && lst.Length > 1 && lst2.Length > 1
    then 
        let rec f (lst: List<int>) (lst2: List<int>) = 
            if lst.Length > lst2.Length
            then 1
            elif lst.Length < lst2.Length
            then 2
            else
                match lst, lst2 with
                | [], [] -> 0
                | hd :: tl, hd2 :: tl2 ->
                    if hd > hd2
                    then 1
                    elif hd < hd2
                    then 2
                    else f tl tl2
                | hd :: tl, [] -> 1
                | [], hd :: tl -> 2
        let rec f2 lst =
            match lst with 
            | [] -> [0]
            | hd :: tl ->
                if hd = 0
                then f2 tl
                else hd :: tl
        let sum = ref 0
        let rec add lst lst2 x s s2 = 
            match lst, lst2 with
            | [], [] -> [x]                              
            | hd :: tl, hd2 :: tl2 ->
                    sum := hd * s + hd2 * s2 + x
                    if !sum > 9
                    then (!sum % 10) :: (add tl tl2 (!sum / 10) s s2)
                    elif !sum < 10 && !sum >= 0
                    then !sum :: (add tl tl2 (!sum / 10) s s2)
                    else (!sum + 10) :: (add tl tl2 -1 s s2)
            | hd :: tl, [] 
            | [], hd :: tl -> 
                    sum := hd * s + x
                    if !sum > 9
                    then (!sum % 10) :: (add tl [] (!sum / 10) s s2)
                    elif !sum < 10 && !sum >= 0
                    then !sum :: (add tl [] (!sum / 10) s s2)
                    else (!sum + 10) :: (add tl [] -1 s s2)
        let f3 lst = 
            lst |> List.rev |> f2 
        let x = f lst.Tail lst2.Tail
        if (lst.Head > lst2.Head) && (x = 1)
        then lst.Head :: (add (List.rev lst.Tail) (List.rev lst2.Tail) 0 lst.Head lst2.Head |> f3)                 
        elif (lst.Head < lst2.Head) &&  (x = 1)
        then lst.Head :: (add (List.rev lst.Tail) (List.rev lst2.Tail) 0 lst2.Head lst.Head |> f3)
        elif (lst.Head < lst2.Head) && (x = 2)
        then lst2.Head :: (add (List.rev lst2.Tail) (List.rev lst.Tail) 0 lst2.Head lst.Head |> f3)            
        elif (lst.Head = lst2.Head) && ((x = 2) || (x = 0) || (x = 1))
        then lst2.Head :: (add (List.rev lst2.Tail) (List.rev lst.Tail) 0 1 1 |> f3)
        elif (lst.Head > lst2.Head) && (x = 2)
        then lst2.Head :: (add (List.rev lst2.Tail) (List.rev lst.Tail) 0 lst.Head lst2.Head |> f3)  
        else 1 :: [0]
    else failwith "error. wrong input"

main [1; 9; 9] [1; 1; 1] |> printfn "%A"
main [1; 8; 4] [-1; 7; 5] |> printfn "%A"
main [1;0] [1; 3; 4] |> printfn "%A"
main [1; 2; 3] [-1; 6; 2; 3] |> printfn "%A"
main [-1; 9] [1; 8] |> printfn "%A"
main [1; 1; 0] [-1; 9] |> printfn "%A"

