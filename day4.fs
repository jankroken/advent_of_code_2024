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

type Pos = int*int 

let toMap (s:string list) : Map<Pos,char> =
    let rec toMap (y:int) (s:string list) =
        match s with
        | [] -> []
        | s::rest ->
            let xs = s |> Seq.toList |> List.indexed |> List.map (fun p -> ((fst p,y),(snd p)))
            printfn $"{xs}"
            xs :: (toMap (y+1) rest) 
    toMap 0 s |> List.concat |> Map.ofList 

let cmap = toMap input 

let xs = cmap |> Map.filter (fun _ k -> k = 'X') |> Map.toList |> List.map fst 

xs |> List.map (printfn "%A")

let xmas1 (x:int,y:int) : bool =
    let m = cmap.TryFind (x+1,y)
    let a = cmap.TryFind (x+2,y)
    let s = cmap.TryFind (x+3,y)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas2 (x:int,y:int) : bool =
    let m = cmap.TryFind (x+1,y+1)
    let a = cmap.TryFind (x+2,y+2)
    let s = cmap.TryFind (x+3,y+3)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas3 (x:int,y:int) : bool =
    let m = cmap.TryFind (x+1,y-1)
    let a = cmap.TryFind (x+2,y-2)
    let s = cmap.TryFind (x+3,y-3)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas4 (x:int,y:int) : bool =
    let m = cmap.TryFind (x-1,y)
    let a = cmap.TryFind (x-2,y)
    let s = cmap.TryFind (x-3,y)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas5 (x:int,y:int) : bool =
    let m = cmap.TryFind (x-1,y+1)
    let a = cmap.TryFind (x-2,y+2)
    let s = cmap.TryFind (x-3,y+3)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas6 (x:int,y:int) : bool =
    let m = cmap.TryFind (x-1,y-1)
    let a = cmap.TryFind (x-2,y-2)
    let s = cmap.TryFind (x-3,y-3)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas7 (x:int,y:int) : bool =
    let m = cmap.TryFind (x,y+1)
    let a = cmap.TryFind (x,y+2)
    let s = cmap.TryFind (x,y+3)
    m = Some('M') && a = Some('A') && s = Some('S')

let xmas8 (x:int,y:int) : bool =
    let m = cmap.TryFind (x,y-1)
    let a = cmap.TryFind (x,y-2)
    let s = cmap.TryFind (x,y-3)
    m = Some('M') && a = Some('A') && s = Some('S')
    
let g1 = xs |> List.filter (xmas1) |> List.length
let g2 = xs |> List.filter (xmas2) |> List.length
let g3 = xs |> List.filter (xmas3) |> List.length
let g4 = xs |> List.filter (xmas4) |> List.length
let g5 = xs |> List.filter (xmas5) |> List.length
let g6 = xs |> List.filter (xmas6) |> List.length
let g7 = xs |> List.filter (xmas7) |> List.length
let g8 = xs |> List.filter (xmas8) |> List.length

let ans1 = g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 

printfn $"{g1} {g2} {g3} {g4} {g5} {g6} {g7} {g8} : {ans1}"


let asx = cmap |> Map.filter (fun _ k -> k = 'A') |> Map.toList |> List.map fst 

let xmas (x:int,y:int) : bool =
    let lr1 = (cmap.TryFind (x-1,y-1)) = Some('M') && (cmap.TryFind (x+1,y+1)) = Some('S')
    let lr2 = (cmap.TryFind (x+1,y+1)) = Some('M') && (cmap.TryFind (x-1,y-1)) = Some('S')
    let rl1 = (cmap.TryFind (x+1,y-1)) = Some('M') && (cmap.TryFind (x-1,y+1)) = Some('S')
    let rl2 = (cmap.TryFind (x-1,y+1)) = Some('M') && (cmap.TryFind (x+1,y-1)) = Some('S')
    (lr1 || lr2) && (rl1 || rl2) 

let xmasses = asx |> List.filter xmas

xmasses |> List.length |> printfn "%A"
