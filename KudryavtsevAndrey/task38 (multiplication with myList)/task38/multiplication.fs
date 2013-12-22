module Multi

open AccessoryFunctions 
open myType 
open listLength

let main list1 list2 = 

    let rec correctionCheck lst = 
        match lst with
        | Empty -> true
        | Lst (h, t) -> h < 10 && correctionCheck t  

    let rec deletingZeroes lst = 
            match lst with 
            | Empty -> Lst (0, Empty) 
            | Lst (hd, tl) -> 
                if hd = 0 
                then deletingZeroes tl 
                else Lst (hd, tl) 

    let list1Tail = myListTail list1
    let list2Tail = myListTail list2
    let list1Head = myListHead list1 
    let list2Head = myListHead list2 
    let tail1Rev = myListReverse list1Tail
    let tail2Rev = myListReverse list2Tail
    
    if lstLength list1 < 2 || lstLength list2 < 2 
    then failwith "Error! Entered incorrect numbers. " 
    elif abs (myListHead list1) <> 1 || abs (myListHead list2) <> 1
    then failwith "Error! Entered incorrect sign, sign must be valued 1 or -1. "
    elif not (correctionCheck list1Tail && correctionCheck list2Tail)
    then failwith "Error! Entered incorrect digits, digits must be valued from 0 to 9. "
    else  
                                                               
        let rec addition lst1 lst2 excess sign1 sign2 =
            match lst1, lst2 with
            | Empty, Empty -> 
                if excess = 0 
                then Empty
                else Lst (excess, Empty)
            | Lst (hd1, tl1), Lst (hd2, tl2) ->                
                let sum1 = hd1 * sign1 + hd2 * sign2 + excess 
                if sum1 > 9 
                then Lst (sum1 % 10, addition tl1 tl2 (sum1 / 10) sign1 sign2) 
                else Lst (sum1, addition tl1 tl2 (sum1 / 10) sign1 sign2)                                   
            | Lst (hd1, tl1), Empty
            | Empty, Lst (hd1, tl1) ->                 
                let sum2 = hd1 + excess 
                if sum2 > 9 
                then Lst (sum2 % 10, addition tl1 Empty (sum2 / 10) sign1 sign2)
                else Lst (sum2, addition tl1 Empty (sum2 / 10) sign1 sign2)

        let rec multiplication hd list exc discharge =
            if discharge > 0
            then Lst (0, multiplication hd list exc (discharge - 1))
            else
                match list with
                | Empty ->
                    if exc > 0 
                    then Lst (exc, Empty)
                    else Lst (0, Empty) 
                | Lst (hd1, tl1) ->                   
                    let mult = hd * hd1 + exc
                    if mult > 9 
                    then Lst (mult % 10, multiplication hd tl1 (mult / 10) 0)
                    else Lst (mult, multiplication hd tl1 0 0)

        let rec multi list1 list2 disch =
            match list2 with
            | Empty -> Empty
            | Lst (hd1, tl1) ->
                addition (multiplication hd1 list1 0 disch) (multi list1 tl1 (disch + 1) ) 0 1 1 

        if list1Head = list2Head
        then Lst (1, multi tail1Rev tail2Rev 0 |> myListReverse |> deletingZeroes)
        else Lst (-1 ,multi tail1Rev tail2Rev 0 |> myListReverse |> deletingZeroes)


let lst1 = Lst (1, Lst (1, Lst (0, Lst (0, Empty))))
let lst2 = Lst (-1, Lst (1, Lst (0, Lst (0, Empty))))
let lst3 = Lst (1, Lst (0, Empty))


printfn "res = %A" (main lst1 lst2) 
printfn "res = %A" (main lst1 lst1) 
printfn "res = %A" (main lst1 lst3) 
printfn "res = %A" (main lst2 lst3) 
printfn "res = %A" (main lst3 lst3) 
