open System.IO

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

let inputs = input |> splitOnLine (fun t -> t = "")

input |> List.map (printfn "%A")

let parse (s:string) =
    let s = s.Split(":")
    let testValue = s[0] |> int64
    let nums = s[1].Split(" ") |> Array.toList |> List.filter (fun i -> i <> "")
               |> List.map (fun s -> s |> int64)
    testValue,nums

let equations = input |> List.map parse

let checkpres (testValue:int64, values:int64 list) =
    let rec check (acc:int64) (values:int64 list) =
        printfn $"check {testValue} {acc} {values}"
        match values with
        | [] -> acc = testValue
        | [a] -> acc+a = testValue
        | a::b::rest ->
            (check (acc+a) (b::rest)) || (check acc ((a*b)::rest))
    check 0L values

let conc a b = $"{a}{b}" |> int64

let check1 (testValue:int64, values:int64 list) =
    let rec check (acc:int64) (values:int64 list) =
        match values with
        | [] -> acc = testValue
        | [a] -> acc+a = testValue || acc*a = testValue
        | a::b::rest ->
            (check (acc+a) (b::rest)) || (check (acc*a) (b::rest))
    check values[0] values.Tail 

let check2 (testValue:int64, values:int64 list) =
    let rec check (acc:int64) (values:int64 list) =
        printfn $"check {testValue} {acc} {values}"
        match values with
        | [] -> acc = testValue
        | [a] -> acc+a = testValue || acc*a = testValue || conc acc a = testValue 
        | a::b::rest ->
            (check (acc+a) (b::rest)) || (check (acc*a) (b::rest)) || (check (conc acc a) (b::rest))
    check values[0] values.Tail 

equations |> List.last |> printfn "%A"

let ok1 = equations |> List.filter check1

ok1 |> List.map (printfn "OK1 %A")

ok1 |> List.map fst |> List.sum |> printfn "ANSWER 1: %A"

let ok2 = equations |> List.filter check2
ok2 |> List.map fst |> List.sum |> printfn "ANSWER 2: %A"
