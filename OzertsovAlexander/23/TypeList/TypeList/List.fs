module MyType

type MyList =
| Lst of int * MyList
| Empty


let list = Lst (1, Lst(25, Lst(100, Lst(12, Empty))))
list |> printfn "%A"