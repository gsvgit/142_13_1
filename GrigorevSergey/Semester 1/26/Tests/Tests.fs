open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    Assert.That (hashAll ["111"; "222"; "222"], Is.EqualTo ([147; 150; 150]))

[<Test>]
let Test2 () =
    Assert.That (hashAll [""], Is.EqualTo ([0]))

[<Test>]
let Test3 () =
    Assert.That (hashAll [], Is.EqualTo ([]))
    
[<Test>]
let Test4 () =
    Assert.That (hashAll ["0"; ""; "azaza"], Is.EqualTo ([48; 0; 535]))