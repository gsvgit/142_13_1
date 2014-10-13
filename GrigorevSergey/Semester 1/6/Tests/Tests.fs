open Problem.Main
open NUnit.Framework
open System

[<Test>]
let Test1 () = Assert.That (main [|0; 1|] 0 1, Is.EqualTo ([|1; 0|]))

[<Test>]
let Test2 () = Assert.That (main [|1; 0|] 1 1, Is.Not.EqualTo ([|0; 1|]))

[<Test>]
let Test3 () = Assert.That (main [|0; 1; 2; 3; 4|] 4 1, Is.EqualTo ([|0; 4; 2; 3; 1|]))

[<Test>]
let Test4 () = Assert.Throws<IndexOutOfRangeException> (fun () -> main [||] 4 5 |> ignore) |> ignore