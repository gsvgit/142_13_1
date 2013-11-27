let multiplication lst1 lst2 = 
    if List.exists (fun x -> x >= 10) (List.tail lst1) = true || List.exists (fun x -> x >= 10) (List.tail lst2) = true || List.exists (fun x -> x <= 0) (List.tail lst1) = true || List.exists (fun x -> x <= 0) (List.tail lst2) = true
    then failwith "Error! You entered wrong digit. Digit must be from 0 to 9."
    elif List.isEmpty (List.tail lst1) = true || List.isEmpty (List.tail lst2) = true
    then failwith "Error! You entered at least one empty list."
    elif List.head lst1 <> 1 && List.head lst1 <> -1 || List.head lst2 <> 1 && List.head lst2 <> -1
    then failwith "Error! You entered wrong sign (first element in every list). Sign must be equal 1 or -1"
    else
        let lst1Rev = List.rev (List.tail lst1)
        let lst2Rev = List.rev (List.tail lst2)
        let acyclic_convolution list1 list2 = 
            let almost = [for i in 0..List.length list2 - 1 -> [for j in 0..List.length list1 + List.length list2 - 2 -> if j >= i && j <= i + List.length list1 - 1
                                                                                                                         then list2.[i] * list1.[j - i]                                                                                                     
                                                                                                                         else 0]]
            let k = Array.zeroCreate (List.length list1 + List.length list2 - 1)
            for j in 0..List.length list1 + List.length list2 - 2 do
                for i in 0..List.length list2 - 1 do k.[j] <- k.[j] + almost.[i].[j]
            k
        let summing (m: int array) v = 
            for i in 0..v do
                if m.[i] >= 10 && i < v
                then 
                    m.[i + 1] <- m.[i + 1] + m.[i] / 10
                    m.[i] <- m.[i] - (m.[i] / 10) * 10
                if i = v
                then m.[i] <- m.[i] + m.[i - 1] / 10
            (List.head lst1 * List.head lst2)::Array.toList (Array.rev m)
        if List.length lst1Rev < List.length lst2Rev
        then 
            let check = (summing (acyclic_convolution lst1Rev lst2Rev) (List.length lst1Rev + List.length lst2Rev - 2))
            if check.[1] >= 10
            then List.head check::check.[1] / 10::check.[1] - (check.[1] / 10) * 10::List.tail (List.tail check)
            else check
        else 
            let check = (summing (acyclic_convolution lst2Rev lst1Rev) (List.length lst1Rev + List.length lst2Rev - 2))
            if check.[1] >= 10
            then List.head check::check.[1] / 10::check.[1] - (check.[1] / 10) * 10::List.tail (List.tail check)
            else check
multiplication [1; 1; 2; 3] [1; 1] |> printfn "%A"
multiplication [-1; 6; 3; 4; 5; 6; 7; 8; 9] [1; 2; 3; 4; 5; 6; 7; 8; 9] |> printfn "%A"
multiplication [10; 1; 2; 3] [-1; 4; 5; 6; 9; 0; 4] |> printfn "%A"
