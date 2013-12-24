module  Addition


open typeList
open reverse
open length


let main lst lst2 = 
    let rec check lst = 
        match lst with
        | Lst (int, myList) ->
                 if int < 10 
                 then check myList 
                 else false
        | Empty -> true

    let h (lst: myList) = 
        match lst with
        | Lst (hd, tl) -> hd
        | Empty -> 0 

    if (h lst = 1 || h lst = -1) && (h lst2 = 1 || h lst2 = -1) &&
       check lst && check lst2 && lengthList lst > 1 && lengthList lst2 > 1
    then 
        let rec f4 lst lst2 = 
            if lengthList lst > lengthList lst2
            then 1 
            elif lengthList lst < lengthList lst2
            then 2
            else
                match lst, lst2 with
                | Empty, Empty -> 0 
                | Lst (hd, tl), Lst (hd2, tl2) -> 
                    if hd > hd2 
                    then 1 
                    elif hd < hd2 
                    then 2 
                    else f4 tl tl2 
                | Lst (hd, tl), Empty -> 1
                | Empty, Lst (hd2, tl2) -> 2
                    
        let rec f2 lst = 
            match lst with 
            | Empty -> Lst (0, Empty) 
            | Lst (hd, tl) -> 
                if hd = 0 
                then f2 tl 
                else Lst (hd, tl) 
        let sum = ref 0        
        let rec add l1 l2 x s s2 = 
            match l1, l2 with 
            | Empty, Empty -> 
                if x = 0
                then Empty
                else Lst (x, Empty)
            | Lst (hd1, tl1), Lst (hd2, tl2) -> 
                sum := hd1 * s + hd2 * s2 + x 
                if !sum > 9 
                then Lst ((!sum % 10), (add tl1 tl2 (!sum / 10) s s2)) 
                elif !sum < 10 && !sum >= 0 
                then Lst (!sum, (add tl1 tl2 (!sum / 10) s s2))
                else Lst ((!sum + 10), (add tl1 tl2 -1 s s2))
            | Empty, Lst (hd, tl) 
            | Lst (hd, tl), Empty -> 
                sum := hd * 1 + x 
                if !sum > 9 
                then Lst (!sum % 10, (add tl Empty (!sum / 10) s s2))
                else Lst (!sum, (add tl Empty (!sum / 10) s s2)) 
                 
        let f3 lst = 
           lst |> f |> f2

        let t (lst: myList) = 
            match lst with 
            | Lst (hd, tl) -> tl 
            | Empty -> Empty

        let lstTail = t lst
        let lst2Tail = t lst2
        let lstHead = h lst
        let lst2Head = h lst2
        let tailRev = f lstTail
        let tail2Rev = f lst2Tail

        let x = f4 lstTail lst2Tail
        if x = 1 && lstHead > lst2Head 
        then Lst (lstHead, (add tailRev tail2Rev 0 lstHead lst2Head |> f3))
        elif x = 1 && lstHead < lst2Head
        then Lst (lstHead, (add tailRev tail2Rev 0 lst2Head lstHead |> f3))
        elif x = 2 && lstHead < lst2Head
        then Lst (lst2Head, (add tail2Rev tailRev 0 lst2Head lstHead |> f3))
        elif (lstHead = lst2Head) && (x = 2 || x = 0 || x = 1)
        then Lst (lst2Head, (add tail2Rev tailRev 0 1 1 |> f3))
        elif x = 2 && lstHead > lst2Head
        then Lst (lst2Head, (add tail2Rev tailRev 0 lstHead lst2Head |> f3))
        else Lst (1, Lst (0, Empty)) 
    else failwith "error. incorrect input"

main (Lst (1, Lst (1, Lst (0, Lst (1, Empty))))) (Lst (-1, Lst (1, Lst (0, Empty)))) |> printfn "%A"
main (Lst (1, Lst (1, Empty))) (Lst (1, Lst (1, Empty))) |> printfn "%A"
main (Lst (1, Lst (7, Empty))) (Lst (1, Lst (4, Empty))) |> printfn "%A"

