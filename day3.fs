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
        | _, _, _ -> failwith $"Not handled: compl={completed} curr={curr} rest={rem}"

    accumulate [] [] lines

let l = input |> splitOnLine (fun t -> t = "")

let split (s: string) : int64 list =
    let s = s.Split " " |> Array.toList |> List.map (fun s -> s |> int64)
    printfn $"{s.Length}"
    s

input |> List.map (printfn "%A")

let mulRE = "mul\((-?[0-9]+),(-?[0-9]+)\)"

let matches (s: string) =
    seq {
        for m in Regex.Matches(s, mulRE) do
            yield m.Groups[1], m.Groups[2]
    }

let x = matches "blahmul(1,2)blehmul(33,-4)bluhmul(mul(2,))))))"

// input |> Seq.toList |> List.map (printfn "%A")

let m = input |> List.map matches |> List.map Seq.toList

m |> List.map (printfn "%A")

let n =
    m
    |> List.concat
    |> List.map (fun i -> ((fst i).ToString() |> int) * ((snd i).ToString() |> int))


n |> List.map (printfn "%A")

let ans1 = n |> List.sum

printfn $"ANSWER 1: {ans1}"
// let muls = input |> matches |> Seq.toList

type ResT =
    | DO
    | DONT
    | MUL of int * int

let mulRE2 = "(mul\((-?[0-9]+),(-?[0-9]+)\)|do\(\)|don't\(\))"

let matches2 (s: string) =
    seq {
        for m in Regex.Matches(s, mulRE2) do
                // printfn $"{m.Value}"
                match m.Value with
                | "do()" -> DO
                | "don't()" -> DONT
                | _ ->
                    // printfn $"{m.Groups[1]}"
                    MUL(m.Groups[2].ToString() |> int, m.Groups[3].ToString() |> int)
    }

let m2 = input |> List.map matches2 |> List.map Seq.toList 

// m2 |> List.map (printfn "%A")

let rec exec (sum:int) (enabled:bool) (inst:ResT list) =
    match inst with
    | [] -> sum
    | DO::rest -> exec sum true rest
    | DONT::rest -> exec sum false rest
    | MUL(a,b)::rest ->
        let sum = if enabled then sum + (a*b) else sum
        exec sum enabled rest 

let program = m2 |> List.concat

let result = exec 0 true program 

printfn $"ANS2: {result}"
