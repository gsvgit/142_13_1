open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    let lst = [1; 2; 3; 4; 5]
    let mlst = myListFromList lst
    Assert.That (myListLength mlst, Is.EqualTo (5))

[<Test>]
let Test2 () =
    let lst = []
    let mlst = myListFromList lst
    Assert.That (myListLength mlst, Is.EqualTo (0))

[<Test>]
let Test3 () =
    let lst = [1]
    let mlst = myListFromList lst
    Assert.That (myListLength mlst, Is.EqualTo (1))