let main lst =
    let rec hashall lst =
        match lst with
        | hd :: tl -> Hash.main hd :: hashall tl
        | [] -> []
    hashall lst

printfn "%A" (main ["apa"; "qwer"; "1wer24rf"; ""])