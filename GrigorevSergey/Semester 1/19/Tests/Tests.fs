open Problem.Main
open NUnit.Framework

[<Test>]
let Test0 () =
    let f = 0.
    let bits = "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    Assert.That (main f, Is.EqualTo (bits))

[<Test>]
let Test1 () =
    let f = 1.
    let bits = "00111111 11110000 00000000 00000000 00000000 00000000 00000000 00000000"
    Assert.That (main f, Is.EqualTo (bits))

[<Test>]
let Test2 () =
    let f = -1.
    let bits = "10111111 11110000 00000000 00000000 00000000 00000000 00000000 00000000"
    Assert.That (main f, Is.EqualTo (bits))

[<Test>]
let Test3 () =
    let f = 0.25
    let bits = "00111111 11010000 00000000 00000000 00000000 00000000 00000000 00000000"
    Assert.That (main f, Is.EqualTo (bits))

[<Test>]
let Test4 () =
    let f = -0.9990234375
    let bits = "10111111 11101111 11111000 00000000 00000000 00000000 00000000 00000000"
    Assert.That (main f, Is.EqualTo (bits))