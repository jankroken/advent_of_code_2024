open System
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

let toMap (s: string list)  =
    let rec toLL (y:int) (s: string list) =
        match s with
        | [] -> []
        | s::rest ->
            let s = s.ToCharArray() |> Array.toList |> List.indexed
                        |> List.map (fun (x,c) -> (x,y),c)
            s :: (toLL (y+1) rest)
    let s = s |> List.rev
    let s = toLL 0 s
    s |> List.concat |> Map.ofList

let map = toMap input

printfn $"{map}"

let isInMap (p:int*int)  = map.ContainsKey  p

let freqAntennas = map |> Map.toList
                   |> List.groupBy (fun (_,c) -> c)
                   |> List.filter (fun (i,_) -> i <> '.')
                   |> List.map (fun (c,pl) -> (c, pl |> List.map fst))

freqAntennas |> printfn "%A"

let allPairs (l: 'a list) : ('a*'a) list =
    let rec allPairs (l: 'a list) =
        match l with
        | [] -> []
        | [_] -> []
        | a::b::rest ->
            let l0 = [(a,b)]
            let l1 = allPairs (a::rest)
            let l2 = allPairs (b::rest)
            [l0;l1;l2] |> List.concat
    allPairs l 

let pairs = freqAntennas |> List.map (fun (pos,a) -> pos,allPairs a)

pairs |> List.map (printfn "Pair: %A")

let ans ((x1,y1),(x2,y2)) : (int*int) list =
    let p1 = 2*x1 - x2, 2*y1 - y2
    let p2 = 2*x2 - x1, 2*y2 - y1
    [p1;p2] |> List.filter isInMap 
    
let corr = pairs |> List.map snd |> List.concat |> List.map ans

corr |> List.map (printfn "%A")

let ans1 = corr |> List.concat |> List.sort |> Set.ofList
let ans1_1 = ans1.Count 

ans1.Count |> printfn "ANSWER1 %A"

input |> List.map (printfn "%A")

let pairs2 =
    pairs
    |> List.map snd
    |> List.concat 
    |> List.map (fun ((x1,y1),(x2,y2)) -> ((x1 |> int64, y1 |> int64),(x2 |> int64,y2 |> int64)))
    
let height = input.Length
let width = input[0].Length

let rangepos = seq { 0 .. (min height width) } |> Seq.map (int64) |> Seq.toList
let rangeneg = rangepos |> List.map (fun x -> -x)
let range = [rangepos;rangeneg] |> List.concat |> List.sort 

range |> List.map (printfn "%A")

let isInMap2 (x:int64,y: int64) =
    if x < 0 || y < 0 then false
    elif x > (width+1 |>int64) || y>(height+1 |> int64) then false
    else
        let x = x |> int
        let y = y |> int
        map.ContainsKey (x,y)

let ans2 ((x1,y1),(x2,y2)) : (int64*int64) list =
    let ans2 (i:int64) = 
        let x = x1 + i*(x2-x1)
        let y = y1 + i*(y2-y1)
        (x,y)
    range |> List.map ans2 |> List.filter isInMap2 
        
// x = x + n * (x2-x1)
// y = y + n * (y2-y1)

let antis2 = pairs2 |> List.map ans2

let antiset = antis2 |> List.concat |> Set.ofList


antiset.Count |> printfn "ANSWER2: %A"
