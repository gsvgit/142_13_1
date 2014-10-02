open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    Assert.That (main "111", Is.EqualTo (147))

[<Test>]
let Test2 () =
    Assert.That (main "222", Is.EqualTo (150))

[<Test>]
let Test3 () =
    Assert.That (main "", Is.EqualTo (0))
    
[<Test>]
let Test4 () =
    Assert.That (main "0", Is.EqualTo (48))