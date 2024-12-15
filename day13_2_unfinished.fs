open System.IO

let print a = a |> printfn "%A"
let printlist a = List.map print
let inputFile = "/Users/jan/downloads/input-nav.txt"
let input = File.ReadAllLines inputFile |> Array.toList

type Button = int64 * int64
type Prize = int64 * int64
type Machine = Button * Button * Prize

let rec gcd x y = if y = 0L then x else gcd y (x % y)

let gcd2 (x1, y1) (x2, y2) =
    let gx = gcd x1 x2
    let gy = gcd y1 y2
    gcd gx gy

let lowestOf (a: Option<int64>, b: Option<int64>) =
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
    let a = block[0].Split("+") |> Array.tail
    let a = a[0].Split(",") |> Array.head |> int64, a[1] |> int64
    let b = block[1].Split("+") |> Array.tail
    let b = b[0].Split(",") |> Array.head |> int64, b[1] |> int64
    let prize = block[2].Split("=") |> Array.tail
    let prize = prize[0].Split(",") |> Array.head |> int64, prize[1] |> int64
    a, b, prize

let machines = blocks |> List.map parseBlock

machines |> List.map (printfn "MACHINE: %A")

let adjustment = 10000000000000L

let machines2 =
    machines
    |> List.map (fun (a, b, (px, py)) -> a, b, (px + adjustment, py + adjustment))


let gcd3 (a:int64, b:int64, c:int64) =
    let ab = gcd a b
    let ac = gcd a c
    let abc = gcd ab ac 
    printfn $"ab={ab} ac={ac} abc={abc}"
    abc
    
let shrink_by_gcd3 (a,b,c) =
    let div = gcd3 (a,b,c)
    (a/div,b/div,c/div)

let solve (limit:bool) (ac:int64,bc:int64) (machine:Machine) =
    let ((x1,y1),(x2,y2),(xp,yp)) = machine
    let b = if (x1*y1-y2*x1) = 0 then None
            else Some ((xp*y1-yp*x1)/(x2*y1-y2*x1))
    let a =
        if b = None then None
        elif (x1-y1) = 0 then None
        else 
        Some (((b.Value*(y2-x2)) + (xp-yp)) / (x1-y1))
 
    if a.IsNone then None
    elif limit && (a.Value > 100L || b.Value > 100L) then None 
    else 
        let success = a.Value*x1 + b.Value*x2 = xp 
        printfn $"a={a.Value} b={b.Value} tokens={a.Value*3L+b.Value} success={success}"
        if success then Some (a.Value*ac+b.Value*bc) else None 
    
// solve2 ((94L,34L),(22L,67L),(8400L,5400L))

let swap (a,b) = (b,a)

let solve1 (a,b,c) =
    let s1 = solve true (3,1) (a,b,c)
    let s2 = solve true (1,3) (swap b,swap a,swap c)
    printfn $"SOLUTIONS {s1} {s2}"
    match s1,s2 with
    | None,None -> None
    | Some(s1),Some(s2) -> Some(min s1 s2)
    | _ ->
        printfn $"****** WEIRD ***** {s1},{s2}"
        None
    
let result1 = machines |> List.map solve1 |> List.choose id |> List.sum |> printfn "ANSWER 1: %A"
