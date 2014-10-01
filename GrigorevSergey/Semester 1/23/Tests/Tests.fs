open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    let lst = [1; 2; 3; 4; 5]
    let ml1 = myListFromList lst
    let ml2 = Node(1,Node(2,Node(3,Node(4,Node(5,Leaf)))))
    Assert.That(ml1, Is.EqualTo(ml2))

[<Test>]
let Test2 () =
    let arr = [|1; 2; 3; 4; 5|]
    let ml1 = myListFromArray arr
    let ml2 = Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
    Assert.That(ml1, Is.EqualTo(ml2))

[<Test>]
let Test3 () =
    let lst = []
    let ml1 = myListFromList lst
    let ml2 = Leaf
    Assert.That(ml1, Is.EqualTo(ml2))

[<Test>]
let Test4 () =
    let arr = [||]
    let ml1 = myListFromArray arr
    let ml2 = Leaf
    Assert.That(ml1, Is.EqualTo(ml2))