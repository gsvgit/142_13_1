module Multiplication

let main (lst: List<int>) (lst2: List<int>) =
    let rec check lst = 
        match lst with
        | hd :: tl -> 
             if (-10 < hd) && (hd < 10) 
             then check tl
             else false
        | [] -> true
    let rec f lst = 
        match lst with
        | [] -> [0]
        | hd :: tl -> 
                if hd = 0
                then f tl
                else hd :: tl

    if (lst.Length < 2 || lst2.Length < 2) &&
       (abs(lst.Head) <> 1 || abs(lst2.Head) <> 1) &&
       (not (check lst.Tail && check lst2.Tail))
    then failwith "error. incorrect input"
    else 
        let sum = ref 0 
        let rec add lst lst2 x s s2 = 
            match lst, lst2 with
            | [], [] -> 
                    if x = 0
                    then []
                    else [x]
            | hd :: tl, hd2 :: tl2 -> 
                    sum := hd * s + hd * s2 + x
                    if !sum > 9
                    then !sum % 10 :: add tl tl2 (!sum / 10) s s2
                    else !sum :: add tl tl2 (!sum / 10) s s2
            | hd :: tl, []
            | [], hd :: tl ->
                    sum := hd + x
                    if !sum > 9
                    then !sum % 10 :: add tl [] (!sum / 10) s s2
                    else !sum :: add tl [] (!sum / 10) s s2 
        let m = ref 0
        let rec mult hd lst x r = 
            if r > 0
            then 0 :: mult hd lst x (r - 1)
            else
                match lst with
                | [] ->
                    if x > 0
                    then [x]
                    else []  
                | hd1 :: tl1 ->
                     m := hd * hd1 + x
                     if !m > 9
                     then !m % 10 :: mult hd tl1 (!m / 10) 0
                     else !m :: mult hd tl1 0 0
        let rec f2 lst lst2 x = 
            match lst2 with
            | [] -> []
            | hd :: tl ->
                    add (mult hd lst 0 x) (f2 lst tl (x + 1)) 0 1 1
        if lst.Head = lst2.Head
        then 1 :: (f2 (List.rev lst.Tail) (List.rev lst2.Tail) 0 |> List.rev |> f)
        else -1 :: (f2 (List.rev lst.Tail) (List.rev lst2.Tail) 0 |> List.rev |> f)
                                            

main [1; 0] [1; 2; 2] |> printfn "%A"
main [-1; 1; 1] [1; 3; 3; 3] |> printfn "%A"














