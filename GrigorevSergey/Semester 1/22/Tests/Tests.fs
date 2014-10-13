open Problem.Main
open NUnit.Framework
open System

[<Test>]
let Test1 () =
    let a = []
    let b = [||]
    Assert.That (mainConvert a, Is.EqualTo (b))

[<Test>]
let Test2 () =
    let a = [1]
    let b = [|1|]
    Assert.That (mainConvert a, Is.EqualTo (b))

[<Test>]
let Test3 () =
    let a = [1; 1]
    let b = [|1; 1|]
    Assert.That (mainConvert a, Is.EqualTo (b))

[<Test>]
let Test4 () =
    let a = [1; 2; 3; 4; 5; 6; 7]
    let b = [|1; 2; 3; 4; 5; 6; 7|]
    Assert.That (mainConvert a, Is.EqualTo (b))