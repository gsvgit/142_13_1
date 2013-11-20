let rand = new System.Random() 

let genRand () =
    let x = rand. NextDouble()
    let y = rand.NextDouble()
    let z = x/y
    z |> printfn "%A"
    
genRand ()

printfn ">>>"
for i in 0 .. 6 do
    16 >>> i |> printfn "%A"

printfn "<<<"
for i in 0 .. 6 do
    16 <<< i |> printfn "%A"

printfn "&&&"
for i in 0 .. 20 do
    16 &&& i |> printfn "%A"

printfn "|||"
for i in 0 .. 6 do
    16 ||| i |> printfn "%A"

printfn "~~~16=%A" (~~~16u)

System.BitConverter.GetBytes(-34.454353) 
|> printfn "%A"