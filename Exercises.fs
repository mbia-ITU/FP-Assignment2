open System

//Exercise 2.1
let rec downto1 n = 
    if n > 0
    then n :: (downto1 (n-1))
    else []

let rec downto2 =
    function
    | n when n > 0 -> n :: (downto2 (n-1))
    | _ -> []

//Exercise 2.2
let removeOddIdx (xs: 'a list) = [for i in 0..2..xs.Length-1 ->[i]]

//Exercise 2.3
let combinePair (xs: 'a list) = [for i in 1..2..xs.Length-1 -> (xs.[i-1], xs.[i])]

//Exercise 2.4
type complex = float * float

let mkComplex x y : complex = (x,y)

let complexToPair (com: complex) = (fst com, snd com)

let inline (|+|) c1 c2 :complex = mkComplex (fst c1 + fst c2) (snd c1 + snd c2)
let inline (|-|) (c1:complex) (c2:complex) = c1 |+| mkComplex (-(fst c2)) (-(snd c2))
let inline (|*|) c1 c2 :complex = mkComplex ((fst c1 * fst c2) - (snd c1 * snd c2)) ((snd c1 * fst c2) + (fst c1 * snd c2))
let inline (|/|) c1 ((a2, b2): complex) :complex = c1 |*| mkComplex (a2 / (a2**2. + b2**2.)) (((-b2) / (a2**2. + b2**2.)))

((mkComplex (-3.3) 10.3) |/| (mkComplex (-3.2) (-2.0)))

//Exercise 2.5
let explode1 (s:string) = [for c in s -> c]

let rec explode2 (s: string) = 
    match s with
    | s when s.Length > 0 -> s.[0] :: explode2 s.[1..]
    | _ -> []


//Exercise 2.6
let implode (list: char list) = 
    String.Concat list

let implode2 (list: char list) :string = 
    List.fold (fun s c2 -> s + c2.ToString() ) "" list

let implodeRev (list: char list) :string = 
    List.fold (fun s c2 -> c2.ToString() + s ) "" list

//Exercise 2.7
let toUpper (s:string) = s.ToUpper()

//Exercise 2.8
let rec ack (m,n) = 
    match m with
    | 0 -> n+1
    | m when m> 0 && n=0 -> ack (m-1, 1)
    | m when m> 0 && n > 0 -> ack (m-1, ack (m,n-1))
    | _ -> 0