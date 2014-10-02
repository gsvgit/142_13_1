open Problem.Main
open NUnit.Framework
open System

[<Test>]
let Test1 () = Assert.That (main 1, Is.EqualTo ([|0; 1|]))

[<Test>]
let Test2 () = Assert.That (main 0, Is.EqualTo ([|0|]))

[<Test>]
let Test3 () = Assert.Throws<ArgumentOutOfRangeException> (fun () -> main -4 |> ignore) |> ignore

[<Test>]
let Test4 () = Assert.That (main 5, Is.EqualTo ([|0; 1; 1; 2; 3; 5|]))