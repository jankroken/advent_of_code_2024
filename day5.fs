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

let inputs = input |> splitOnLine (fun l -> l = "")

let toPair (s:string) =
    s.Split "|" |> Array.toList |> (fun [a;b] -> (a |> int,b |> int))

let rules = inputs[0] |> List.map toPair 
let updates = inputs[1] |> List.map (fun s -> s.Split(",") |> Array.toList |> List.map (fun s -> s |> int))

// rules |> List.map (printfn "%A")

// updates |> List.map (printfn "%A")

let fulfills (update: int list) (order: (int*int)) : bool =
    let t1 = update |> List.tryFindIndex (fun e -> e = (fst order))
    let t2 = update |> List.tryFindIndex (fun e -> e = (snd order))
    match (t1,t2) with
        | (Some i1), (Some i2) when i1 > i2 -> false
        | _ -> true
        
let fulfillsAll (order: (int*int) list) (update: int list)  =
    order |> List.map (fulfills update) |> List.filter (fun i -> i = false) |> List.isEmpty
    
let ok = updates |> List.filter (fulfillsAll rules)
let bad = updates |> List.filter (fun s -> fulfillsAll rules s |> not)

let middle (l:int list) =
    l[l.Length / 2]

printfn """---"""    
    
// ok |> List.map (printfn "%A")

ok |> List.map middle |> List.sum |> printfn "Answer 1: %A"

// bad |> List.map (printfn "%A")

let relevant (l:int list) (r:int*int) =
    let hasFirst = l |> List.contains (fst r)
    let hasSecond = l |> List.contains (snd r)
    hasFirst && hasSecond 

let rec swap (l:int list) (a:int,b:int) =
    match l with
    | [] -> []
    | e::rest when e = a -> b::(swap rest (a,b))
    | e::rest when e = b -> a::(swap rest (a,b))
    | e::rest -> e :: (swap rest (a,b))

let rec fix (l:int list) =
    let violates (r:int*int) = fulfills l r |> not
    match rules |> List.filter violates with
    | [] -> l
    | (a,b)::rest ->
        let ia = l |> List.findIndex (fun i -> i = a)
        let ib = l |> List.findIndex (fun i -> i = b)
        let s = swap l (a,b)
        fix s 
        
        
let fixd = bad |> List.map fix

// fixd |> List.map (printfn "%A")

fixd |> List.map middle |> List.sum |> printfn "Answer 2: %A"
    
   
