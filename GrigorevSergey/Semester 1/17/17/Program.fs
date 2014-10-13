namespace Problem

module LongInteger = 
    type Sign =
    | Plus
    | Minus
    
    type LongInt =
    | Number of Sign * list<int>
    | Zero
                
    let multLists a b =
        let rec addByDigit a b acc =
            match a, b with
            | h1 :: t1, h2 :: t2 -> List.append acc [h1 + h2] |> addByDigit t1 t2
            | h1 :: t1, [] -> List.append acc [h1] |> addByDigit t1 []
            | [], h2 :: t2 -> List.append acc [h2] |> addByDigit [] t2
            | [], [] -> acc
        let rec multiplyByNumber a number acc =
            match a with
            | h :: t -> List.append acc [h * number] |> multiplyByNumber t number
            | [] -> acc
        let rec multiply a shift acc =
            match a with
            | h :: t -> 
                let multiplied = List.replicate shift 0 |> multiplyByNumber b h
                addByDigit acc multiplied [] |> multiply t (shift + 1)
            | [] -> acc
        let rec carry lst toCarry acc =
            match lst with
            | h :: t -> List.append acc [(h + toCarry) % 10] |> carry t ((h + toCarry) / 10)
            | [] when toCarry <> 0 -> List.append acc [toCarry % 10] |> carry [] (toCarry / 10)
            | [] -> acc
        let mult = multiply a 0 []
        carry mult 0 []

    let (*) a b =
        match a, b with
        | Zero, _ -> Zero
        | _,  Zero -> Zero
        | Number (Plus, m), Number (Minus, n) -> Number (Minus, multLists m n)
        | Number (Minus, m), Number (Plus, n) -> Number (Minus, multLists m n)
        | Number (Minus, m), Number (Minus, n) -> Number (Plus, multLists m n)
        | Number (Plus, m), Number (Plus, n) -> Number (Plus, multLists m n)