open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () = Assert.That (fib 0, Is.EqualTo (0))

[<Test>]
let Test2 () = Assert.That (fib 1, Is.EqualTo (1))

[<Test>]
let Test3 () = Assert.That (fib 10, Is.EqualTo (55))

[<Test>]
let Test4 () = Assert.That (fib 20, Is.EqualTo (6765))

[<Test>]
let Test5 () = Assert.That (fib -9, Is.EqualTo (34))

[<Test>]
let Test6 () = Assert.That (fib -10, Is.EqualTo (-55))