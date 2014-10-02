open Problem.LongInteger
open NUnit.Framework

[<Test>]
let Test1 () =
    let a = Number (Plus, [9; 1; 1; 1])
    let b = Number (Plus, [9; 2; 3])
    let c = Number (Plus, [1; 5; 1; 8; 6; 3])
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test2 () =
    let a = Number (Minus, [9; 1; 1; 1])
    let b = Number (Plus, [9; 2; 3])
    let c = Number (Minus, [1; 5; 1; 8; 6; 3])
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test3 () =
    let a = Number (Plus, [9; 1; 1; 1])
    let b = Number (Minus, [9; 2; 3])
    let c = Number (Minus, [1; 5; 1; 8; 6; 3])
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test4 () =
    let a = Number (Minus, [9; 1; 1; 1])
    let b = Number (Minus, [9; 2; 3])
    let c = Number (Plus, [1; 5; 1; 8; 6; 3])
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test5 () =
    let a = Zero
    let b = Number (Plus, [9; 2; 3])
    let c = Zero
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test6 () =
    let a = Number (Plus, [9; 1; 1; 1])
    let b = Zero
    let c = Zero
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test7 () =
    let a = Number (Minus, [1; 2; 3; 4; 5])
    let b = Number (Plus, [5; 4; 3; 2; 1])
    let c = Number (Minus, [5; 4; 7; 2; 9; 5; 0; 7; 6])
    Assert.That (a * b, Is.EqualTo (c))