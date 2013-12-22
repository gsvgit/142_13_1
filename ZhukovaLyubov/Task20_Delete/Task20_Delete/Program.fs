let rec filter (lst: int List) (arr: int array) = 
    match lst with
    | [] -> []
    | hd :: tl -> let k = ref 0
                  let i = ref 0
                  while !k = 0 && !i < arr.Length do
                      if arr.[!i] = hd
                      then k := 1
                      else i := !i + 1
                  if !k = 1
                  then filter tl arr
                  else hd :: filter tl arr
printfn "%A" (filter [0; 1; 2; 3; 4; 5] [|10; 2|])
printfn "%A" (filter [1; 5; 8; 6; 3] [||])
printfn "%A" (filter [6; 5; 8; 7; 7; 2] [|6; 7; 9|])