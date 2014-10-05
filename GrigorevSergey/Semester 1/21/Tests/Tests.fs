open Problem.Main
open NUnit.Framework
open System

[<Test>]
let Test1 () =
    let a = [|2; 0; 0|]
    let b = [|1; 1|]
    Assert.That (mainSum a b, Is.EqualTo (mainSumOrig a b))

[<Test>]
let Test2 () =
    let a = [||]
    let b = [||]
    Assert.That (mainSum a b, Is.EqualTo (mainSumOrig a b))

[<Test>]
let Test3 () =
    let a = [||]
    let b = [|1; 1|]
    Assert.That (mainSum a b, Is.EqualTo (mainSumOrig a b))

[<Test>]
let Test4 () =
    let a = [|10; 1; 0; 2|]
    let b = [||]
    Assert.That (mainSum a b, Is.EqualTo (mainSumOrig a b))

[<Test>]
let Test5 () =
    let a = [|1; 2; 3; 4; 5; 6|]
    let b = [|1; 2; 3; 4; 5; 6|]
    Assert.That (mainSum a b, Is.EqualTo (mainSumOrig a b))

[<Test>]
let Test6 () =
    let a = [|-1; -1; -1; -1; -1|]
    let b = [|1|]
    Assert.That (mainSum a b, Is.EqualTo (mainSumOrig a b))