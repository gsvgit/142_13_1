let convert lst = 
    List.toArray lst
printfn "%A" (convert [1; 2; 3; 4; 5])