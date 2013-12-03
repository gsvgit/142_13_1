let hashAll (lst:List<string>) =
    let main (str:string) =
        int (str.[0])
    let rec hash lst1 lst2=
        match lst1 with
        | [] -> lst2
        | h::d -> hash d (main h::lst2)
    List.rev (hash lst [])
printfn "%A" (hashAll ["ld";"fdf";"fdf";"fd"])