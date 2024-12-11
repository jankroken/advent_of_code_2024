open System.IO
open System.Text.RegularExpressions

let inputFile = "input.txt"

let input = File.ReadAllLines inputFile |> Seq.toList

let toInt (s: string) : int64 = s |> int64

let rec splitOnLine (splitOn: 'T -> bool) (lines: 'T list) : 'T list list =
    let rec accumulate (completed: 'T list list) (curr: 'T list) (rem: 'T list) : 'T list list =
        match completed, curr, rem with
        | _, _, a :: rest when splitOn a -> accumulate ((curr |> List.rev) :: completed) [] rest
        | _, _, a :: rest -> accumulate completed (a :: curr) rest
        | _, [], [] -> completed |> List.rev
        | _, _, [] -> (curr |> List.rev) :: completed |> List.rev

    accumulate [] [] lines

let stones = input[0].Split(" ") |> Array.toList |> List.map (int64)
input |> List.map (printfn "%A")

printfn $"STONES {stones}"

let blink (stone: int64) =
    let text = $"{stone}"
    let length = text.Length

    if stone = 0 then
        [ 1L ]
    elif length % 2 = 0 then
        let left = text.Substring(0, length / 2) |> int64
        let right = text.Substring(length / 2, length / 2)

        let right =
            if right |> int64 = 0L then
                0L
            else
                Regex.Replace(right, "^0*", "") |> int64

        [ left; right ]
    else
        [ stone * 2024L ]

let blinkall (stones: int64 list) = stones |> List.map blink |> List.concat

let at7 =
    stones
    |> blinkall
    |> blinkall
    |> blinkall
    |> blinkall
    |> blinkall
    |> blinkall
    |> blinkall

let rec blinkn (times: int) (stones: int64 list) =
    if times = 0 then
        stones
    else
        blinkn (times - 1) (stones |> blinkall)

let at25 = blinkn 25 stones

printfn $"ANSWER1: {at25.Length}"

let distinct25 =
    at25
    |> List.groupBy id
    |> List.map snd
    |> List.map (fun l -> l.Length |> int64, l.Head)

let blinknn (n: int) (c: int64, stone: int64) : (int64 * int64) list =
    let stones = [ stone ] |> blinkn n |> List.groupBy id |> List.map snd
    let stones = stones |> List.map (fun l -> l.Length |> int64, l.Head)
    let stones = stones |> List.map (fun l -> (fst l) * c, snd l)
//     printfn $"{stones}"
    stones 

blinknn 4 (5L, 5L)

let compress (stones: (int64*int64) list list) =
    let stones = stones |> List.concat
    stones |> printfn "%A"
    let stones = stones |> List.groupBy snd
    let stones = stones |> List.map (fun vc -> snd vc |> List.map fst |> List.sum, fst vc)
    // stones |> printfn "%A"
    stones 

let x40 = distinct25 |> List.map (blinknn 15) |> compress 

let x55 = x40 |> List.map (blinknn 15) |> compress
let x65 = x55 |> List.map (blinknn 10) |> compress
let x75 = x65 |> List.map (blinknn 10) |> compress

x75 |> List.map fst |> List.sum |> printfn "ANSWER 2: %A"
