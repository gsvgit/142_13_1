let multiplication (lst1: int List) (lst2: int List) = 
    if (List.exists (fun x -> x >= 10) (List.tail lst1) = true) || (List.exists (fun x -> x >= 10) (List.tail lst2) = true) || (List.exists (fun x -> x <= 0) (List.tail lst1) = true) || (List.exists (fun x -> x <= 0) (List.tail lst2) = true)
    then failwith "Error! You entered wrong digit. Digit must be from 0 to 9."
    elif (List.isEmpty (List.tail lst1) = true) || (List.isEmpty (List.tail lst2) = true)
    then failwith "Error! You entered at least one empty list."
    elif ((List.head lst1 <> 1) && (List.head lst1 <> -1)) || ((List.head lst2 <> 1) && (List.head lst2 <> -1))
    then failwith "Error! You entered wrong sign (first element in every list). Sign must be equal 1 or -1"
    else
        let rvlst1 = List.rev (List.tail lst1)
        let rvlst2 = List.rev (List.tail lst2)
        let acyclic_convolution list1 list2 = 
            let short = ref 0
            let long = ref 0
            short := List.length list1
            long := List.length list2
            let almost = [for i in 0..!long - 1 -> let a = ref 1
                                                   [for j in 0..!short + !long - 2 -> if j >= i && j <= i + !short - 1
                                                                                      then a := !a + 1
                                                                                           list2.[i] * list1.[!a - 2]                                                                                                     
                                                                                      else 0]]
            let k = Array.zeroCreate (!short + !long - 1)
            for j in 0..!short + !long - 2 do
                for i in 0..!long - 1 do k.[j] <- k.[j] + almost.[i].[j]
            k
        let summing (m: int array) v = 
            for i in 0..v do
                if m.[i] >= 10 && i < v
                then 
                    m.[i + 1] <- m.[i + 1] + m.[i] / 10
                    m.[i] <- m.[i] - ((m.[i]/10) * 10)
                if i = v
                then m.[i] <- m.[i] + m.[i - 1] / 10
            (List.head lst1 * List.head lst2)::Array.toList (Array.rev m)
        if List.length rvlst1 < List.length rvlst2
        then (summing (acyclic_convolution rvlst1 rvlst2) (List.length rvlst1 + List.length rvlst2 - 2))
        else (summing (acyclic_convolution rvlst2 rvlst1) (List.length rvlst1 + List.length rvlst2 - 2))
multiplication [1; 1; 2; 3] [1; 1] |> printfn "%A"
multiplication [-1; 2; 3; 4; 5; 6; 7; 8; 9] [1; 2; 3; 4; 5; 6; 7; 8; 9] |> printfn "%A"
multiplication [1; 1; 2; 3] [-1; 4; 5; 6; 9; 10; 4] |> printfn "%A"
