open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    let tr = Node (" ", [Leaf ("  ")])
    Assert.That (sumOfLengths tr, Is.EqualTo (3))

[<Test>]
let Test2 () =
    let tr = Leaf ("")
    Assert.That (sumOfLengths tr, Is.EqualTo (0))

[<Test>]
let Test3 () =
    let tr = Node ("foo", [Node ("aaa", [Leaf ("fdf"); Leaf ("123")]); Node ("", [Leaf ("f"); Leaf ("12")])])
    Assert.That (sumOfLengths tr, Is.EqualTo (15))

[<Test>]
let Test4 () =
    let tr = Node (" ", [Node ("123", [Node ("234", [Leaf ("5656"); Leaf ("5656"); Leaf ("000")]); Leaf ("434")]); Leaf ("  ")])
    Assert.That (sumOfLengths tr, Is.EqualTo (23))