type MyPerfectList =
| Lst of int * MyPerfectList
| Empty

let lst = Lst(42, Lst(57, Lst(67, Empty)))

printfn "%A" lst
