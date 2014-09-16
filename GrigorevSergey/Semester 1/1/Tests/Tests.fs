// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Problem1.Main
open NUnit.Framework

[<Test>]
let Test1 () = Assert.That(main 0, Is.EqualTo(1))

[<Test>]
let Test2 () = Assert.That(main 1, Is.EqualTo(5))

[<Test>]
let Test3 () = Assert.That(main 2, Is.EqualTo(31))