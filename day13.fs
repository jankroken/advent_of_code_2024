open System.IO

type INT = System.Numerics.BigInteger
let print a = a |> printfn "%A"
let printlist a = List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> List.map (printfn "%A")

type Button = INT * INT
type Prize = INT * INT
type Machine = Button * Button * Prize

let rec gcd x y = if y = 0L then x else gcd y (x % y)

let gcd2 (x1, y1) (x2, y2) =
    let gx = gcd x1 x2
    let gy = gcd y1 y2
    gcd gx gy

let lowestOf (a: Option<INT>, b: Option<INT>) =
    match a, b with
    | None, None -> None
    | None, Some(c) -> Some(c)
    | Some(c), None -> Some(c)
    | Some(a), Some(b) -> Some(min a b)

let split (l: string list) =
    let rec split (acc: string list list) (l: string list) =
        match (acc, l) with
        | acc, [] -> acc |> List.map List.rev |> List.rev
        | acc, i :: rest when i = "" -> split ([] :: acc) rest
        | [], i :: rest -> split [ [ i ] ] rest
        | a :: acc, i :: rest -> split ((i :: a) :: acc) rest

    split [] l

let blocks = split input

let parseBlock (block: string list) : Machine =
    printfn $"PARSEBLOCK {block}"
    let a = block[0].Split("+") |> Array.tail
    let a = a[0].Split(",") |> Array.head |> INT.Parse, a[1] |> INT.Parse
    let b = block[1].Split("+") |> Array.tail
    let b = b[0].Split(",") |> Array.head |> INT.Parse, b[1] |> INT.Parse
    let prize = block[2].Split("=") |> Array.tail
    let prize = prize[0].Split(",") |> Array.head |> INT.Parse, prize[1] |> INT.Parse
    a, b, prize

let machines = blocks |> List.map parseBlock

let adjustment = 10000000000000I

let machines2 =
    let add z (x, y) = (x + z, y + z)
    machines |> List.map (fun (a, b, p) -> a, b, p |> add adjustment)

let solve (ignoreLimit:bool) (((x1,y1),(x2,y2),(xp,yp)):Machine) =
    let b = (yp*x1 - xp*y1) / (y2*x1 - x2*y1)
    let a = (xp - b*x2) / x1
    let score = a*3I + b
   
    let t1 = xp = x1*a + x2*b
    let t2 = yp = y1*a + y2*b
    let t3 = a <= 100I && a >= 0I
    let t4 = b <= 100I && a >= 0I 
    // printfn $"TEST {t1} {t2} ignore?:{ignoreLimit} {t3} {t4}"
    if t1 && t2 && (ignoreLimit || (t3 && t4)) then 
        score
    else
        0
   
let scores1 = machines |> List.map (solve false)
let scores2 = machines2 |> List.map (solve true)

// scores |> List.map (printfn "SCORE: %A")

let answer1 = scores1 |> List.sum
let answer2 = scores2 |> List.sum
printfn $"Answer 1: {answer1}"
printfn $"Answer 2: {answer2}"
