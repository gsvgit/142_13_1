let main (inArr: int array) = 
    if inArr = [||] || inArr.Length = 1         
    then inArr
    else
        let rec qSort low high = 
            let l = ref low
            let r = ref high
            let m = inArr.[(low + high) / 2]
            while !l <= !r do
                while inArr.[!l] < m do l := !l + 1
                while inArr.[!r] > m do r := !r - 1
                if (!l <= !r) 
                then 
                    let buf = inArr.[!l]
                    inArr.[!l] <- inArr.[!r]
                    inArr.[!r] <- buf
                    l := !l + 1
                    r := !r - 1
            if low < !r 
            then qSort low !r
            if !l < high 
            then qSort !l high
        qSort 0 (inArr.Length - 1)
        inArr    

let arr1 = [|-3; 56; 0; 0; -100; 44646; 4|]
let arr2 = [|-45; -67; -867; -97111|] 
        
printfn "%A" (main arr1)
printfn "%A" (main arr2)    
printfn "%A" (main [|1|])
printfn "%A" (main [||])