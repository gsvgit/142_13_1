open Problem.Main
open NUnit.Framework
open System

[<Test>]
let Test1 () = Assert.That (main [|0; 1|], Is.EqualTo ([|0; 1|]))

[<Test>]
let Test2 () = Assert.That (main [|1; 0|], Is.EqualTo ([|0; 1|]))

[<Test>]
let Test3 () = Assert.That (main [|1; 0; 4; 5; 3; 6; 2|], Is.EqualTo ([|0; 1; 2; 3; 4; 5; 6|]))

[<Test>]
let Test4 () =
    let a = Array.zeroCreate<int> 1000
    let r = new Random ()
    for i in [0 .. 999] do
        a.[i] <- r.Next (1000)
    let b = Array.map (fun x -> x) a
    array.Sort b
    Assert.That (main a, Is.EqualTo (b))

[<Test>]
let Test5 () = Assert.Throws<NullReferenceException> (fun () -> main null |> ignore) |> ignore

[<Test>]
let Test6 () = Assert.That (main [||], Is.EqualTo ([||]))