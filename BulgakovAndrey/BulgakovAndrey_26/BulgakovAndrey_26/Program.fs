let hashAll (lst:List<string>) =
    let rec hash lst1 lst2 =
        match lst1 with
        | [] -> lst2
        | h :: d -> hash d (Hashfun.main h :: lst2)
    List.rev (hash lst [])
printfn "%A" (hashAll ["ld"; "fdf"; "fdf"; "fd"])