open TypeList
let length (lst:MyList) =
    let k = ref 0
    let rec leng (list:MyList) l =
        match list with
        | Lst(int, MyList) -> leng MyList (l + 1) 
        | Empty -> l
    leng lst !k     
        
let k = Lst(1, Lst(2, Empty))
printfn "%A" (length k)