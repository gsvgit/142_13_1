namespace Problem

module Main = 
    let main (a: list<int>) (b: list<int>) =
        let rec addByDigit (a: list<int>) (b: list<int>) (acc: list<int>) =
            match a, b with
            | h1 :: t1, h2 :: t2 -> List.append acc [h1 + h2] |> addByDigit t1 t2
            | h1 :: t1, [] -> List.append acc [h1] |> addByDigit t1 []
            | [], h2 :: t2 -> List.append acc [h2] |> addByDigit [] t2
            | [], [] -> acc
        let rec carry n (lst: list<int>) =
            match lst with
            | h :: t when h + n > 9 -> h + n - 10 :: (carry 1 t)
            | h :: t -> h + n :: (carry 0 t)
            | [] ->
                if n = 0
                then lst
                else List.append lst [n]
        addByDigit a b [] |> carry 0