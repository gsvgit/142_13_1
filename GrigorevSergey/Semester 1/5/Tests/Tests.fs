open Problem.Main
open NUnit.Framework
open System

[<Test>]
let Test1 () = Assert.That (main [|0; 1|], Is.EqualTo ([|1; 0|]))

[<Test>]
let Test2 () = Assert.That (main [|1; 0|], Is.EqualTo ([|0; 1|]))

[<Test>]
let Test3 () = Assert.Throws<ArgumentException> (fun () -> main [||] |> ignore) |> ignore 