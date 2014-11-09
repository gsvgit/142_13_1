namespace Problem

open System

module BigIntegers = 
    type Sign =
    | Plus
    | Minus
    | Zero
    
    type BigInteger (lst : list<int>, sgn) =
        let rec compare (a : BigInteger) (b : BigInteger) =
            let alst = a.List
            let blst = b.List
            let asgn = a.Sgn
            let bsgn = b.Sgn
            let rec comp1 lst1 lst2 =
                match lst1, lst2 with
                | [], [] -> 0
                | _, [] -> 1
                | [], _ -> -1
                | h1 :: [], h2 :: [] -> sign (h1 - h2)
                | _ :: t1, _ :: t2 -> comp1 t1 t2
            let rec comp lst1 lst2 curr =
                match lst1, lst2 with
                | [], [] -> curr
                | _, [] -> 1
                | [], _ -> -1
                | h1 :: t1, h2 :: t2 -> comp t1 t2 (if h1 = h2 then curr else sign (h1 - h2))
            match alst, blst, asgn, bsgn with
            | _, _, Zero, Zero -> 0
            | _, _, Zero, Plus -> -1
            | _, _, Zero, Minus -> 1
            | _, _, _, Zero -> -compare b a
            | _, _, Plus, Minus -> 1
            | _, _, Minus, Plus -> -1
            | m, n, Minus, Minus -> comp n m 0
            | m, n, Plus, Plus -> comp m n 0
        new (n) =
            let rec iter n lst =
                if n = 0
                then lst
                else iter (n / 10) ((n % 10) :: lst)
            match n with
            | 0 -> BigInteger ([], Zero)
            | n when n > 0 -> BigInteger (iter n [] |> List.rev, Plus)
            | n -> BigInteger (iter -n [] |> List.rev, Minus)
        interface IComparable with
            member this.CompareTo (other) =
                let a = other :?> BigInteger
                compare this a
        interface IComparable<BigInteger> with
            member this.CompareTo (other) =
                compare this other
        override this.ToString () =
            let ss =
                match sgn with
                | Minus -> "-"
                | Zero -> "0"
                | _ -> ""
            let rec collect s l =
                if List.length l = 0
                then s
                else collect (l.Head.ToString () + s) l.Tail
            ss + collect "" lst
        override this.Equals (other) =
            let a = other :?> BigInteger
            compare this a = 0
        override this.GetHashCode () =
            lst.GetHashCode ()
        member this.Sgn = sgn
        member this.Sign =
            match sgn with
            | Plus -> 1
            | Minus -> -1
            | Zero -> 0
        member this.List = lst
        member this.IsEven =
            match this.Sgn with
            | Zero -> true
            | _ -> lst.[0] % 2 = 0
        member this.IsOdd = not this.IsEven
        member this.Half =
            let l = List.rev lst
            let rec iter c l acc =
                match l with
                | [] -> acc
                | h :: t -> 
                    let n = c * 10 + h
                    (n / 2) :: (if acc = [0] then [] else acc) |> iter (n % 2) t
            match this.Sgn with
            | Zero -> BigInteger.Zero
            | _ -> new BigInteger (iter 0 l [], sgn)

        member this.ToInt32 () = Int32.Parse (this.ToString ())
        
        static member Zero = new BigInteger (0)
        static member One = new BigInteger (1)
        static member MinusOne = new BigInteger (-1)
        
        static member Abs (elem : BigInteger) = if elem.Sgn = Minus then -elem else elem

        static member (>) (a, b) =
            compare a b > 0
       
        static member (>=) (a, b) =
            compare a b >= 0
        
        static member (<) (a, b) =
            b > a
        
        static member (<=) (a, b) =
            b >= a
                        
        static member (=) (a, b) =
            compare a b = 0
       
        static member (<>) (a, b) =
            compare a b <> 0
        
        static member (~-) (a : BigInteger) =
            match a.Sgn with
            | Zero -> a
            | Plus -> new BigInteger (a.List, Minus)
            | Minus -> new BigInteger (a.List, Plus)

        static member (+) (a : BigInteger, b : BigInteger) =
            let alst = a.List
            let blst = b.List
            let asgn = a.Sgn
            let bsgn = b.Sgn
            let addLists a b =
                let rec addByDigit a b acc =
                    match a, b with
                    | h1 :: t1, h2 :: t2 -> List.append acc [h1 + h2] |> addByDigit t1 t2
                    | h1 :: t1, [] -> List.append acc [h1] |> addByDigit t1 []
                    | [], h2 :: t2 -> List.append acc [h2] |> addByDigit [] t2
                    | [], [] -> acc
                let rec carry n lst =
                    match lst with
                    | h :: [] when h + n = 0 -> []
                    | h :: t when h + n > 9 -> -10 + h + n :: (carry 1 t)
                    | h :: t when h + n < 0 -> 10 + h + n :: (carry -1 t)
                    | h :: t -> h + n :: (carry 0 t)
                    | [] ->
                        if n = 0
                        then lst
                        else List.append lst [n]
                addByDigit a b [] |> carry 0
            match alst, blst, asgn, bsgn with
            | _, _, Zero, Zero -> a
            | _, _, Zero, _ -> b
            | _, _, _, Zero -> a
            | _, _, Minus, Plus -> b + a
            | _, _, Minus, Minus -> -(-a + -b)
            | m, n, Plus, Plus -> new BigInteger (addLists m n, Plus)
            | m, n, Plus, Minus ->
                match compare a -b with
                | 0 -> new BigInteger (0)
                | -1 -> new BigInteger (addLists (List.map (fun elem -> -elem) m) n, Minus)
                | 1 -> new BigInteger (addLists (List.map (fun elem -> -elem) n) m, Plus)
                | _ -> failwith "Comparison function returned inapropriate value"

        static member (-) (a : BigInteger, b : BigInteger) = a + -b

        static member (*) (a : BigInteger, b : BigInteger) =
            let alst = a.List
            let blst = b.List
            let asgn = a.Sgn
            let bsgn = b.Sgn
            let multLists a b =
                let rec addByDigit a b acc =
                    match a, b with
                    | h1 :: t1, h2 :: t2 -> List.append acc [h1 + h2] |> addByDigit t1 t2
                    | h1 :: t1, [] -> List.append acc [h1] |> addByDigit t1 []
                    | [], h2 :: t2 -> List.append acc [h2] |> addByDigit [] t2
                    | [], [] -> acc
                let rec multiplyByNumber a number acc =
                    match a with
                    | h :: t -> List.append acc [h * number] |> multiplyByNumber t number
                    | [] -> acc
                let rec multiply a shift acc =
                    match a with
                    | h :: t -> 
                        let multiplied = List.replicate shift 0 |> multiplyByNumber b h
                        addByDigit acc multiplied [] |> multiply t (shift + 1)
                    | [] -> acc
                let rec carry lst toCarry acc =
                    match lst with
                    | h :: t -> List.append acc [(h + toCarry) % 10] |> carry t ((h + toCarry) / 10)
                    | [] when toCarry <> 0 -> List.append acc [toCarry % 10] |> carry [] (toCarry / 10)
                    | [] -> acc
                let mult = multiply a 0 []
                carry mult 0 []
            match alst, blst, asgn, bsgn with
            | _, _, Zero, _ -> new BigInteger (0)
            | _, _, _, Zero -> new BigInteger (0)
            | m, n, s, t when s = t -> new BigInteger (multLists m n, Plus)
            | m, n, _, _ -> new BigInteger (multLists m n, Minus)

    let (|BigInteger|) (input : BigInteger) = (input.Sgn, input.List)
