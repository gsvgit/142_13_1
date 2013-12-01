let stringHash (str: string) = 
    let hash = ref 0
    for i in 0..str.Length - 1 do
        hash := (!hash + int (str.[i]) * i) % 342
    !hash
                      
stringHash "HelloWorld !!1!11" |> printfn "%A"
stringHash "WorldHello !!1!11" |> printfn "%A"
stringHash "HelloMyWorld !!1!11" |> printfn "%A"
stringHash "hfgfhfsfd;j;ldj;lsd;l.cjsfujpg[pflsd;l;sl'JDF/SDLJCSDHCW475RO'0R" |> printfn "%A"
stringHash "" |> printfn "%A"
