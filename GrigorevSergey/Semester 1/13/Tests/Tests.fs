open NUnit.Framework
open Problem.Fibonacci

module Test =
    let check n =
        let rec iter i =
            if i > n
            then None
            elif fib9 i <> fib11 i
            then Some (i)
            else iter (i + 1)
        iter 0


    [<Test>]
    //[<Ignore>]
    let Test () =
        let n = 10000
        Assert.That (check n, Is.EqualTo (None))

    [<Test>]
    //[<Ignore>]
    let Test1 () =
        let n = 10000000
        Assert.That (fib11 n, Is.EqualTo (fib9 n))