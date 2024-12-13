open System.IO

let print a = a |> printfn "%A"
let printlist a = List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> List.map (printfn "%A")

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

blocks |> List.map (printfn "%A")

let parseBlock (block: string list) : Machine =
    printfn $"PARSEBLOCK {block}"
    let a = block[0].Split("+") |> Array.tail
    let a = a[0].Split(",") |> Array.head |> int64, a[1] |> int64
    let b = block[1].Split("+") |> Array.tail
    let b = b[0].Split(",") |> Array.head |> int64, b[1] |> int64
    let prize = block[2].Split("=") |> Array.tail
    let prize = prize[0].Split(",") |> Array.head |> int64, prize[1] |> int64
    a, b, prize

let machines = blocks |> List.map parseBlock

machines |> List.map (printfn "MACHINE: %A")

let solve (limit: bool) ((a, b, prize): Machine) : Option<int64> =
    printfn $"solve {limit} {a} {b} -> {prize}"
    let skip = fst b / (gcd2 a b)

    let rec solve (bsol: Option<int64>) (ac: int64) (prize: Prize) : Option<int64> =
        // printfn $"solve {bsol} {ac} {prize}"
        let removeA (n: int64) : Prize =
            (fst prize) - ((fst a) * n), (snd prize) - ((snd a) * n)

        let canPushB =
            let divisible = ((fst prize) % (fst b) = 0L) && ((snd prize) % (snd b) = 0L)
            let bc1 = (fst prize) / (fst b)
            let bc2 = (snd prize) / (snd b)
            divisible && (bc1 = bc2)

        if limit && ac > 100L then
            printfn "LIMIT"
            bsol
        elif fst prize < 0L || snd prize < 0L then
            bsol
        elif prize = (0L, 0L) then
            printfn "END"

            match bsol, ac with
            | None, _ -> Some(ac * 3L)
            | Some(c), _ -> Some(min c (ac * 3L))
        elif canPushB then
            let push_b =
                let bc = (fst prize) / (fst b)
                if limit && bc <= 100L then Some((ac * 3L) + bc) else None

            let bsol = lowestOf (bsol, push_b)

            solve bsol (ac + skip) (removeA skip)
        else
            let newPrize = removeA 1L
            if fst newPrize < 0L || snd newPrize < 01L then
                printfn $"FLUNKING {prize} -1L > {newPrize}"
            solve bsol (ac + 1L) (removeA 1L)

    solve None 0L prize

let solve1 = solve true

let bests = machines |> List.map solve1

bests |> printfn "BEST: %A"

bests
|> List.filter (_.IsSome)
|> List.map (fun (Some(i)) -> i)
|> List.sum
|> printfn "ANSWER 1 %A"

let adjustment = 10000000000000L

let machines2 =
    machines
    |> List.map (fun (a, b, (px, py)) -> a, b, (px + adjustment, py + adjustment))


let solve2 (machine:Machine) =
    let ((x1,y1),(x2,y2),(xp,yp)) = machine
    printfn $"machine: {machine}"

solve2 ((94L,34L),(22L,67L),(8400L,5400L))
