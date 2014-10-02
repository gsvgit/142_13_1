namespace Problem

module LongInteger = 
    type Sign =
    | Plus
    | Minus
    
    type LongInt =
    | Number of Sign * list<int>
    | Zero

    let rec compare a b =
        let rec comp lst1 lst2 =
            match lst1, lst2 with
            | [], [] -> 0
            | _, [] -> 1
            | [], _ -> -1
            | h1 :: [], h2 :: [] -> sign (h1 - h2)
            | _ :: t1, _ :: t2 -> comp t1 t2
        match a, b with
        | Zero, Zero -> 0
        | Zero, Number (Plus, _) -> -1
        | Zero, Number (Minus, _) -> 1
        | n, Zero -> compare Zero n
        | Number (Plus, _), Number (Minus, _) -> 1
        | Number (Minus, _), Number (Plus, _) -> -1
        | Number (Minus, m), Number (Minus, n) -> compare (Number (Plus, n)) (Number (Plus, m))
        | Number (Plus, m), Number (Plus, n) -> comp m n
            
    let addLists a b =
        let rec addByDigit a b acc =
            match a, b with
            | h1 :: t1, h2 :: t2 -> List.append acc [h1 + h2] |> addByDigit t1 t2
            | h1 :: t1, [] -> List.append acc [h1] |> addByDigit t1 []
            | [], h2 :: t2 -> List.append acc [h2] |> addByDigit [] t2
            | [], [] -> acc
        let rec carry n lst =
            match lst with
            | h :: [] when h + n = 0 -> []
            | h :: t when h + n > 9 -> -10 + h + n :: (carry 1 t)
            | h :: t when h + n < 0 -> 10 + h + n :: (carry -1 t)
            | h :: t -> h + n :: (carry 0 t)
            | [] ->
                if n = 0
                then lst
                else List.append lst [n]
        addByDigit a b [] |> carry 0

    let rec (+) a b =
        match a, b with
        | Zero, Zero -> Zero
        | Zero, num -> num
        | num, Zero -> num
        | Number (Minus, _), Number (Plus, _) -> b + a
        | Number (Minus, m), Number (Minus, n) -> Number (Minus, addLists m n)
        | Number (Plus, m), Number (Plus, n) -> Number (Plus, addLists m n)
        | Number (Plus, m), Number (Minus, n) ->
            match compare a (Number (Plus, n)) with
            | 0 -> Zero
            | -1 -> Number (Minus, addLists (List.map (fun elem -> -elem) m) n)
            | 1 -> Number (Plus, addLists (List.map (fun elem -> -elem) n) m)
            | _ -> failwith "Comparison function returned inapropriate value"