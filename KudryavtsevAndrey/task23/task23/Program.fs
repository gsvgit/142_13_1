module myType

type myList =
| Lst of int * myList
| Empty

let list = Lst (1, Lst (2, Lst (3, Lst (4, Empty))))
printfn "%A" list
