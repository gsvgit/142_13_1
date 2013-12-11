module Hash

let stringHash (str: string)= 
    let (masgl: array<char>) = [|'A'; 'E'; 'I'; 'O'; 'U'; 'Y'; 'a'; 'e'; 'i'; 'o'; 'u'; 'y'|]
    let k = ref 1
    let s = ref 0
    for i in str do
        for j in masgl do
            if i = j 
            then k := !k + 1
            else s := int(i) + !s 
    !s % !k

stringHash "Any text" |> printfn "%A" 
