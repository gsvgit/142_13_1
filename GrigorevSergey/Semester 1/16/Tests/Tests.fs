open Problem.LongInteger
open NUnit.Framework

[<Test>]
let Test1 () =
    let a = Number (Plus, [1; 1; 1; 1])
    let b = Number (Plus, [1; 2; 3])
    let c = Number (Plus, [2; 3; 4; 1])
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test2 () =
    let a = Number (Plus, [1])
    let b = Number (Minus, [1])
    let c = Zero
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test3 () =
    let a = Number (Plus, [1; 1; 1; 1])
    let b = Number (Minus, [1; 2; 3])
    let c = Number (Plus, [0; 9; 7])
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test4 () =
    let a = Number (Plus, [5])
    let b = Number (Minus, [9])
    let c = Number (Minus, [4])
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test5 () =
    let a = Number (Plus, [1; 1; 1; 2])
    let b = Number (Minus, [1; 2; 3])
    let c = Number (Plus, [0; 9; 7; 1])
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test6 () =
    let a = Number (Minus, [1; 1; 1; 1])
    let b = Number (Plus, [1; 2; 3])
    let c = Number (Minus, [0; 9; 7])
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test7 () =
    let a = Zero
    let b = Zero
    let c = Zero
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test8 () =
    let a = Number (Minus, [1])
    let b = Zero
    let c = Number (Minus, [1])
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test9 () =
    let a = Zero
    let b = Number (Plus, [1])
    let c = Number (Plus, [1])
    Assert.That (a + b, Is.EqualTo (c))