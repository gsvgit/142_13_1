module Mult
let multiplication lst1 lst2 = 

    let lh1 = List.length lst1
    let lh2 = List.length lst2
    
    let checkDigit lt = 
        List.exists (fun x -> x < 10 || x > 0) lt

    if List.head lst1 <> 1 && List.head lst1 <> -1 || 
       List.head lst2 <> 1 && List.head lst2 <> -1
    then failwith "Error! You entered wrong sign (first element in every list). Sign must be equal 1 or -1"
    
    elif checkDigit lst1 = false ||
         checkDigit lst2 = false
    then failwith "Error! You entered wrong digit. Digit must be from 0 to 9."

    elif lst1 = [] || 
         lst2 = [] ||
         List.tail lst2 = [] ||
         List.tail lst1 = []
    then failwith "Error! You didn't enter at least one number."
    
    else
        let lst1Rev = List.rev (List.tail lst1)
        let lst2Rev = List.rev (List.tail lst2)
        let acyclicConvolution list1 list2 = 
            let l1 = List.length list1
            let l2 = List.length list2
            let lstMult = [for i in 0..l2 - 1 -> [for j in 0..l1 + l2 - 2 -> if j >= i && j <= i + l1 - 1
                                                                             then list2.[i] * list1.[j - i]                                                                                                     
                                                                             else 0]]
            let k = Array.zeroCreate (l1 + l2 - 1)
            for j in 0..l1 + l2 - 2 do
                for i in 0..l2 - 1 do k.[j] <- k.[j] + lstMult.[i].[j]
            k
        let res1 = acyclicConvolution lst1Rev lst2Rev
        let res2 = acyclicConvolution lst2Rev lst1Rev
        let summing (m: int array) v =  
            for i in 0..v do
                if m.[i] >= 10 && i < v
                then 
                    m.[i + 1] <- m.[i + 1] + m.[i] / 10
                    m.[i] <- m.[i] - (m.[i] / 10) * 10
                if i = v
                then m.[i] <- m.[i] + m.[i - 1] / 10
            (List.head lst1 * List.head lst2)::Array.toList (Array.rev m)
        if lh1 = 2 && lh2 = 2
        then 
            if res1.[0] >= 10
            then (List.head lst1 * List.head lst2)::res1.[0] / 10::res1.[0] % 10::[]
            else (List.head lst1 * List.head lst2)::Array.toList res1
        elif lh1 < lh2
        then 
            let check = summing res1 (lh1 + lh2 - 4)
            if check.[1] >= 10
            then List.head check::check.[1] / 10::check.[1] % 10::List.tail (List.tail check)
            else check
        else
            let check = summing res2 (lh1 + lh2 - 4)
            if check.[1] >= 10
            then List.head check::check.[1] / 10::check.[1] % 10::List.tail (List.tail check)
            else check
multiplication [1; 1] [1; 1] |> printfn "%A"
multiplication [-1; 6; 3; 4; 5; 6; 7; 8; 9] [1; 2; 3; 4; 5; 6; 7; 8; 9] |> printfn "%A"
multiplication [10; 1; 2; 3] [-1; 4; 5; 6; 9; 0; 4] |> printfn "%A"
