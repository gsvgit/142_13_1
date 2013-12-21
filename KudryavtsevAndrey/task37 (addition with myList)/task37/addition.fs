module Add

open myType 
open listLength 
open AccessoryFunctions 


let main (list1: myList) (list2: myList) = 
    
    let rec correctionCheck lst = 
        match lst with
        | Empty -> true
        | Lst (h, t) -> h < 10 && correctionCheck t   
    
    if (myListHead list1 = 1 || myListHead list1 = -1) &&
       (myListHead list2 = 1 || myListHead list2 = -1) &&
       correctionCheck list1 = true &&
       correctionCheck list2 = true &&
       lstLength list1 > 1 && lstLength list2 > 1
    then 
    
        let rec listMatching (lst1: myList) (lst2: myList) = 
            if lstLength lst1 > lstLength lst2
            then 1 
            elif lstLength lst1 < lstLength lst2
            then 2
            else
                match lst1, lst2 with
                | Empty, Empty -> 0 
                | Lst (hd1, tl1), Lst (hd2, tl2) -> 
                    if hd1 > hd2 
                    then 1 
                    elif hd1 < hd2 
                    then 2 
                    else listMatching tl1 tl2 
                | Lst (hd1, tl1), Empty -> 1
                | Empty, Lst (hd2, tl2) -> 2
                    
        let rec cuttingZeroes (lst: myList) = 
            match lst with 
            | Empty -> Lst (0, Empty) 
            | Lst (hd, tl) -> 
                if hd = 0 
                then cuttingZeroes tl 
                else Lst (hd, tl) 
                
        let rec addition (l1: myList) (l2: myList) excess sign1 sign2 = 
            match l1, l2 with 
            | Empty, Empty -> Lst (excess, Empty)
            | Lst (hd1, tl1), Lst (hd2, tl2) -> 
                let sum = hd1 * sign1 + hd2 * sign2 + excess 
                if sum > 9 
                then Lst ((sum % 10), (addition tl1 tl2 (sum / 10) sign1 sign2)) 
                elif sum <= 9 && sum >= 0 
                then Lst (sum, (addition tl1 tl2 (sum / 10) sign1 sign2))
                else Lst ((sum + 10), (addition tl1 tl2 -1 sign1 sign2))
            | Empty, Lst (hd, tl) 
            | Lst (hd, tl), Empty -> 
                let sum1 = hd * sign1 + excess 
                if sum1 > 9 
                then Lst (sum1 % 10, (addition tl Empty (sum1 / 10) sign1 sign2))
                elif sum1 <= 9 && sum1 >= 0 
                then Lst (sum1, (addition tl Empty (sum1 / 10) sign1 sign2)) 
                else Lst (sum1 + 10, (addition tl Empty -1 sign1 sign2)) 
            
        let ending (list: myList) = 
           list |> myListReverse |> cuttingZeroes 

        let list1Tail = myListTail list1
        let list2Tail = myListTail list2
        let list1Head = myListHead list1
        let list2Head = myListHead list2
        let tail1Rev = myListReverse list1Tail
        let tail2Rev = myListReverse list2Tail

        let x = listMatching list1Tail list2Tail
        if x = 1 && list1Head > list2Head 
        then Lst (list1Head, (addition tail1Rev tail2Rev 0 list1Head list2Head |> ending))
        elif x = 1 && list1Head < list2Head
        then Lst (list1Head, (addition tail1Rev tail2Rev 0 list2Head list1Head |> ending))
        elif x = 2 && list1Head < list2Head
        then Lst (list2Head, (addition tail2Rev tail1Rev 0 list2Head list1Head |> ending))
        elif (x = 2 && list1Head = list2Head) ||
             (x = 0 && list1Head = list2Head) ||
             (x = 1 && list1Head = list2Head)
        then Lst (list2Head, (addition tail2Rev tail1Rev 0 1 1 |> ending))
        elif x = 2 && list1Head > list2Head
        then Lst (list2Head, (addition tail2Rev tail1Rev 0 list1Head list2Head |> ending))
        else Lst (1, Empty) 
    else failwith "error! wrong numbers format" 
    

let lst1 = Lst (1, Lst (1, Lst (1, Lst (1, Empty))))
let lst2 = Lst (1, Lst (2, Lst (2, Lst (3, Empty))))
let lst3 = Lst (0, Empty) 
let lst4 = Lst (-1, Lst (3, Lst (3, Empty)))
let lst5 = Lst (-1, Lst (6, Lst (7, Empty)))

printfn "res = %A" (main lst1 lst2) 
printfn "res = %A" (main lst4 lst5) 
printfn "res = %A" (main lst3 lst4) 

