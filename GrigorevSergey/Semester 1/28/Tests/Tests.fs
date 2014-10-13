open Problem.Main
open NUnit.Framework

[<Test>]
let Test1 () =
    let tr1 = Node (" ", [Leaf ("  ")])
    let tr2 = Leaf ("")
    let tr3 = Node ("foo", [Node ("aaa", [Leaf ("fdf"); Leaf ("123")]); Node ("", [Leaf ("f"); Leaf ("12")])])
    let tr4 = Node (" ", [Node ("123", [Node ("234", [Leaf ("5656"); Leaf ("5656"); Leaf ("000")]); Leaf ("434")]); Leaf ("  ")])
    let lst = ListNode (tr1, ListNode (tr2, ListNode (tr3, ListNode (tr4, ListLeaf))))
    Assert.That (main lst, Is.EqualTo ([3; 0; 15; 23]))

[<Test>]
let Test2 () =
    let lst = myListFromList [Leaf (""); Leaf ("")]
    Assert.That (main lst, Is.EqualTo ([0; 0]))

[<Test>]
let Test3 () =
    let lst = myListFromList []
    Assert.That (main lst, Is.EqualTo ([]))