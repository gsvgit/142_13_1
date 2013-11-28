type List =
| Lst of int * List
| Empty


let list = Lst (1, Lst(25, Lst(100, Lst(12, Empty))))
list |> printfn "%A"