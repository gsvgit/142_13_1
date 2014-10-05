open NUnit.Framework
open Problem.Main

[<Test>]
let Test1 () = Assert.That (main [|1 .. 10|] 8 100, Is.EqualTo ([|8; 9|]))

[<Test>]
let Test2 () = Assert.That (main [|1 .. 1000|] 8 100, Is.EqualTo ([|8 .. 98|]))

[<Test>]
let Test3 () = Assert.That (main [|1; 3; 8; 4; 10|] 2 8, Is.EqualTo ([|1; 3|]))

[<Test>]
let Test4 () = Assert.That (main [|1 .. 1000|] 8 0, Is.EqualTo ([||]))

[<Test>]
let Test5 () = Assert.That (main [|1 .. 1000|] -5 -3, Is.EqualTo ([||]))