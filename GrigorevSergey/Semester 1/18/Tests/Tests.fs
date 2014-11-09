open Problem.BigIntegers
open Problem.Fibonacci
open NUnit.Framework
open System

// BigInteger tests

[<Test>]
let Test00 () =
    let a = new BigInteger (10)
    let b = new BigInteger (1)
    let c = new BigInteger (11)
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test01 () =
    let a = new BigInteger (5)
    let b = new BigInteger (-7)
    let c = new BigInteger (-2)
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test02 () =
    let a = new BigInteger (0)
    let b = new BigInteger (7)
    let c = new BigInteger (-7)
    Assert.That (a - b, Is.EqualTo (c))

[<Test>]
let Test03 () =
    let a = new BigInteger (-5)
    let b = new BigInteger (5)
    let c = new BigInteger (0)
    Assert.That (a + b, Is.EqualTo (c))

[<Test>]
let Test04 () =
    let a = new BigInteger (-5)
    let b = new BigInteger (5)
    let c = new BigInteger (-25)
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test05 () =
    let a = new BigInteger (-5)
    let b = new BigInteger (0)
    let c = new BigInteger (0)
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test06 () =
    let a = new BigInteger (0)
    let b = new BigInteger (5)
    let c = new BigInteger (0)
    Assert.That (a * b, Is.EqualTo (c))

[<Test>]
let Test07 () =
    let a = new BigInteger (10)
    let b = new BigInteger (12)
    Assert.That (b > a, Is.EqualTo (true))

[<Test>]
let Test08 () =
    let a = new BigInteger (0)
    let b = new BigInteger (5)
    Assert.That (a < b, Is.EqualTo (true))

[<Test>]
let Test09 () =
    let a = new BigInteger (0)
    let b = new BigInteger (0)
    Assert.That (a <> b, Is.EqualTo (false))

[<Test>]
let Test0A () =
    let a = BigInteger.One
    let b = BigInteger.MinusOne
    Assert.That (a <> -b, Is.EqualTo (false))

[<Test>]
let Test0B () =
    let a = BigInteger.Zero
    let b = BigInteger.Zero
    Assert.That (a <> b, Is.EqualTo (false))

// fib7 tests

[<Test>]
let Test70 () =
    let a = BigInteger.Zero
    let b = BigInteger.Zero
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test71 () =
    let a = BigInteger.One
    let b = BigInteger.One
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test72 () =
    let a = new BigInteger (10)
    let b = new BigInteger (55)
    Assert.That (fib8 a, Is.EqualTo (b))
    
[<Test>]
let Test73 () =
    let a = new BigInteger (20)
    let b = new BigInteger (6765)
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test74 () =
    let a = new BigInteger (-10)
    let b = new BigInteger (-55)
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test75 () =
    let a = new BigInteger (-9)
    let b = new BigInteger (34)
    Assert.That (fib8 a, Is.EqualTo (b))

// fib8 tests

[<Test>]
let Test80 () =
    let a = BigInteger.Zero
    let b = BigInteger.Zero
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test81 () =
    let a = BigInteger.One
    let b = BigInteger.One
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test82 () =
    let a = new BigInteger (10)
    let b = new BigInteger (55)
    Assert.That (fib8 a, Is.EqualTo (b))
    
[<Test>]
let Test83 () =
    let a = new BigInteger (20)
    let b = new BigInteger (6765)
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test84 () =
    let a = new BigInteger (-10)
    let b = new BigInteger (-55)
    Assert.That (fib8 a, Is.EqualTo (b))

[<Test>]
let Test85 () =
    let a = new BigInteger (-9)
    let b = new BigInteger (34)
    Assert.That (fib8 a, Is.EqualTo (b))

// fib9 tests

[<Test>]
let Test90 () =
    let a = BigInteger.Zero
    let b = BigInteger.Zero
    Assert.That (fib9 a, Is.EqualTo (b))

[<Test>]
let Test91 () =
    let a = BigInteger.One
    let b = BigInteger.One
    Assert.That (fib9 a, Is.EqualTo (b))

[<Test>]
let Test92 () =
    let a = new BigInteger (10)
    let b = new BigInteger (55)
    Assert.That (fib9 a, Is.EqualTo (b))
    
[<Test>]
let Test93 () =
    let a = new BigInteger (20)
    let b = new BigInteger (6765)
    Assert.That (fib9 a, Is.EqualTo (b))

[<Test>]
let Test94 () =
    let a = new BigInteger (-10)
    let b = new BigInteger (-55)
    Assert.That (fib9 a, Is.EqualTo (b))

[<Test>]
let Test95 () =
    let a = new BigInteger (-9)
    let b = new BigInteger (34)
    Assert.That (fib9 a, Is.EqualTo (b))

// fib10 tests

[<Test>]
let Test100 () =
    let a = BigInteger.Zero
    let b = BigInteger.Zero
    Assert.That (fib10 a, Is.EqualTo (b))

[<Test>]
let Test101 () =
    let a = BigInteger.One
    let b = BigInteger.One
    Assert.That (fib10 a, Is.EqualTo (b))

[<Test>]
let Test102 () =
    let a = new BigInteger (10)
    let b = new BigInteger (55)
    Assert.That (fib10 a, Is.EqualTo (b))
    
[<Test>]
let Test103 () =
    let a = new BigInteger (20)
    let b = new BigInteger (6765)
    Assert.That (fib10 a, Is.EqualTo (b))

[<Test>]
let Test104 () =
    let a = new BigInteger (-10)
    let b = new BigInteger (-55)
    Assert.That (fib10 a, Is.EqualTo (b))

[<Test>]
let Test105 () =
    let a = new BigInteger (-9)
    let b = new BigInteger (34)
    Assert.That (fib10 a, Is.EqualTo (b))

// fib11 tests

[<Test>]
let Test110 () =
    let a = BigInteger.Zero
    let b = BigInteger.Zero
    Assert.That (fib11 a, Is.EqualTo (b))

[<Test>]
let Test111 () =
    let a = BigInteger.One
    let b = BigInteger.One
    Assert.That (fib11 a, Is.EqualTo (b))

[<Test>]
let Test112 () =
    let a = new BigInteger (10)
    let b = new BigInteger (55)
    Assert.That (fib11 a, Is.EqualTo (b))
    
[<Test>]
//[<Ignore>]
let Test113 () =
    let a = new BigInteger (20)
    let b = new BigInteger (6765)
    Assert.That (fib11 a, Is.EqualTo (b))

[<Test>]
let Test114 () =
    let a = new BigInteger (-10)
    let b = new BigInteger (-55)
    Assert.That (fib11 a, Is.EqualTo (b))

[<Test>]
let Test115 () =
    let a = new BigInteger (-9)
    let b = new BigInteger (34)
    Assert.That (fib11 a, Is.EqualTo (b))

// fib12 tests

[<Test>]
let Test120 () =
    let a = BigInteger.Zero
    let b = BigInteger.One
    Assert.That (fib12 b, Is.EqualTo ([|a; b|]))

[<Test>]
let Test121 () =
    let a = BigInteger.Zero
    Assert.That (fib12 a, Is.EqualTo ([|a|]))

[<Test>]
let Test122 () =
    let a = new BigInteger (-4)
    Assert.Throws<ArgumentOutOfRangeException> (fun () -> fib12 a |> ignore) |> ignore

[<Test>]
let Test123 () =
    let a = BigInteger.Zero
    let b = BigInteger.One
    let c = new BigInteger (2)
    let d = new BigInteger (3)
    let e = new BigInteger (5)
    Assert.That (fib12 e, Is.EqualTo ([|a; b; b; c; d; e|]))
