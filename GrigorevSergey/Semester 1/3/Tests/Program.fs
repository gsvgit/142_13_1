open NUnit.Framework
open Problem3.Main

[<Test>]
let Test1 () = Assert.That(main [|1 .. 10|] 8, Is.EqualTo([|8; 9|]))

[<Test>]
let Test2 () = Assert.That(main [|1 .. 1000|] 8, Is.EqualTo([|8 .. 999|]))