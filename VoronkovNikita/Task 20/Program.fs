let rec mainFilter (lst: int List) (arr: int array) = 
    match lst with
    |[] -> []
    |hd::tl -> let a = ref 0
               let i = ref 0
               while (!a = 0 && !i < Array.length arr) do
                   if arr.[!i] = hd
                   then a := 5
                   else i := !i + 1
               if !a = 5
               then mainFilter tl arr
               else hd::mainFilter tl arr
printfn "%A" (mainFilter [1; 6; 7; 7; 10] [|1; 7|])
printfn "%A" (mainFilter [1; 1; 1; 1; 1; 1] [|1|])
printfn "%A" (mainFilter [1; 2; 3; 4; 5; 6] [|1; 4; 9|])