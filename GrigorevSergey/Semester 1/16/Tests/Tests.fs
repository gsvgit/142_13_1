open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    let a = [1; 1; 1; 1]
    let b = [1; 2; 3]
    let c = [2; 3; 4; 1]
    Assert.That(main a b, Is.EqualTo(c))

[<Test>]
let Test2 () =
    let a = [1]
    let b = []
    let c = [1]
    Assert.That(main a b, Is.EqualTo(c))

[<Test>]
let Test3 () =
    let a = []
    let b = []
    let c = []
    Assert.That(main a b, Is.EqualTo(c))

[<Test>]
let Test4 () =
    let a = [0]
    let b = [0]
    let c = [0]
    Assert.That(main a b, Is.EqualTo(c))

[<Test>]
let Test5 () =
    let a = [9]
    let b = [9]
    let c = [8; 1]
    Assert.That(main a b, Is.EqualTo(c))