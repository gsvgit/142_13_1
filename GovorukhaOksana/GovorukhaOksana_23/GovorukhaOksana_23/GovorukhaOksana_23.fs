module public typeList

type myList = 
| Lst of int * myList
| Empty

let x = Lst (0, Lst (1, Lst (11, Lst (111, Empty))))

x |> printfn "%A"
