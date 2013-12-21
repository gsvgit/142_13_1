open Hash 
open Trees
open typeList

let main (tr: Tree) = 
    let n = ref 0
    let lst1 = ref Empty
    let f2 (lst: myList) (i: int) = 
        match lst with
        | Lst (int, myList) -> Lst (i, Lst (int,myList))  
        | Empty -> Lst (i, Empty) 
    
    
    let rec f (lst: myList) (tr: Tree) =
        
        match tr with
        | Node (str, list) ->  lst1 := f2 lst (Hash.stringHash(str)) 
                               for i in 0 .. (List.length list - 1 ) do 
                                   f (f2 !lst1 (Hash.stringHash(str))) list.[i] 
                                   
        | Leaf (str) -> lst1 := f2 !lst1 (Hash.stringHash(str)) 
    
    f Empty tr

    lst1

main (Node("au", [Leaf "rdooge "; Leaf "aoieughy"])) |> printfn "%A"
    

    

        
        
    
   

    
       
  
   
      