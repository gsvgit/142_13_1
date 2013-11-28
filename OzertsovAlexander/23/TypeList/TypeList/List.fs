type List =
| List of int*List
| Empty


let list = List (1, List(25, List(100, List(12, Empty))))
list |> printfn "%A"