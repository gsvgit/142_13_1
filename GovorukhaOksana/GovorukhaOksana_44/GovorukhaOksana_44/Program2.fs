

type myList = 
    | Lst of int * myList
    | Empty

let f (list: myList) = 
    let l = ref 0
    let lengthList (x: myList) = 
        l := 0
        let rec count (x: myList) = 
            match x with
            | Lst (int, myList) -> l := !l + 1
                                   count myList
            | Empty -> l := !l
        count x 
    lengthList list

    let array = Array.zeroCreate (!l + 1)
    let i = ref 0
    let rec f2 (list: myList) =
        match list with 
        | Lst (int, Lst (int2, myList)) -> array.[!i] <- int
                                           i := !i + 1
                                           f2 (Lst (int2, myList))
        | Lst (int, Empty) -> array.[!i] <- int
                              i := !i + 1
                              f2 Empty
        | Empty -> array.[!i] <- 0 
    f2 list
    
    let rec f3 (list: myList)  =
        match list with 
        | Lst (int, Lst (int2, myList)) ->  i := !i - 1
                                            Lst(array.[!i], f3 (Lst (int2, myList)))
        | _ ->  i := !i - 1
                Lst (array.[!i],Empty)
    f3 list

    
f (Lst(0, Lst(1, Empty))) |> printfn "%A"

        
        


       
       

           
    