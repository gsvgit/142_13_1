module treeFunction

type Tree =
    | Node of string * list<Tree>
    | Leaf of string

let rec main tree =

    let rec roundTree lstTr =
        match lstTr with
        | hd :: tl -> main hd + roundTree tl                         
        | [] -> 0

    let rec vowels str i =
        if i < String.length str
        then 
            if str.[i] = 'a' || str.[i] = 'e' || str.[i] = 'i' || str.[i] = 'o' || str.[i] = 'u' || str.[i] = 'y' 
               || str.[i] = 'A' || str.[i] = 'E' || str.[i] = 'I' || str.[i] = 'O' || str.[i] = 'U' || str.[i] = 'Y'
            then 1 + vowels str (i + 1)
            else vowels str (i + 1) 
        else 0   

    match tree with
    | Node (st, listTree) -> (vowels st 0) + roundTree listTree            
    | Leaf st -> vowels st 0  

printfn "%A" (main (Node ("abc", [Node ("def", [Leaf "hij"; Leaf "klm"; Leaf "nop"]);Node ("qrs", [Leaf "tuv"; Leaf "wm"; Leaf "yz"])])))
         



