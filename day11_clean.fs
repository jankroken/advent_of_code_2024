open System
open System.IO

let inputFile = "/Users/jan/downloads/input-nav.txt"
let input = File.ReadAllLines inputFile |> Array.head

let stones = input.Split(" ") |> Array.toList |> List.map (int64)

printfn $"STONES {stones}" // [0; 7; 6618216 ; ...

let digits (n: int64) =
    let rec digits (acc: int64) (n: int64) =
        if n = 0L then acc else digits (acc + 1L) (n / 10L)

    digits 0L n

let blink (stone: int64) : int64 list =
    let digits = digits stone

    if stone = 0L then
        [ 1L ]
    elif digits % 2L = 0L then
        let filter = pown 10L (digits / 2L |> int)
        let left = stone / filter
        let right = stone % filter
        [ left; right ]
    else
        [ stone * 2024L ]
        
let blinkall (stones: int64 list) = stones |> List.map blink |> List.concat

let rec blinkn (times: int) (stones: int64 list) =
    if times = 0 then
        stones
    else
        stones |> blinkall |> blinkn (times - 1)

let at25 = blinkn 25 stones

printfn $"ANSWER1: {at25.Length}"

// task 2

let stones2 = stones |> List.map (fun stone -> (1L, stone))

let blink2 (count: int64, stone: int64) =
    blink stone |> List.map (fun s -> count, s)

let blinkall2 (stones: (int64 * int64) list) : (int64 * int64) list =
    stones |> List.map blink2 |> List.concat

let rec blinkn2 (n: int) (stones: (int64 * int64) list) =
    if n = 0 then
        stones
    else
        stones |> blinkall2 |> blinkn2 (n - 1)

let compress (stones: (int64 * int64) list) =
    stones
    |> List.groupBy snd
    |> List.map (fun group ->
        let count = snd group |> List.map fst |> List.sum
        let value = fst group
        count, value)

let rec blink_and_compress_n (n:int) (stones:(int64*int64) list) =
    let blink_and_compress stones = stones |> blinkall2 |> compress
    if n = 0 then stones
    else
        stones |> blink_and_compress |> blink_and_compress_n (n-1)

let t1 = DateTimeOffset.Now.ToUnixTimeMilliseconds()
let resultingStones = stones2 |> blink_and_compress_n 75
let t2 = DateTimeOffset.Now.ToUnixTimeMilliseconds()

printfn $"TIME: {t2-t1}"
// resultingStones = [(27906L, 2457L); (27906L, 9456L); (79067L, 20L); (...

resultingStones |> List.map fst |> List.sum |> printfn "ANSWER 2: %A"