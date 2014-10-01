namespace Problem

module Main = 
    let main a b =
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
                let multiplied = multiplyByNumber b h [for i in [0 .. shift - 1] -> 0]
                addByDigit acc multiplied [] |> multiply t (shift + 1)
            | [] -> acc
        let rec carry (lst: list<int>) toCarry (acc: list<int>) =
            match lst with
            | h :: t -> List.append acc [(h + toCarry) % 10] |> carry t ((h + toCarry) / 10)
            | [] when toCarry <> 0 -> List.append acc [toCarry % 10] |> carry [] (toCarry / 10)
            | [] -> acc
        let mult = multiply a 0 []
        carry mult 0 []    