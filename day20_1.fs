open System.IO
open System.Linq

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> printlist

type Pos = int*int 

let parse (input:string list) : Map<Pos,char> =
    let input = input |> List.rev |> List.indexed
    let toXY (y,s:string) =
        let s = s.ToCharArray() |> Array.toList |> List.indexed 
        s |> List.map (fun (x,c) -> (x,y),c)
    input |> List.map toXY |> List.concat |> Map.ofList

let map = parse input
let allPositions = map.Keys |> Seq.toList

map |> print

let neighbours (x:int,y:int) = [x-1,y;x+1,y;x,y-1;x,y+1]

type Cost = int

let startpos = map |> Map.toList |> List.filter (fun (_,c) -> c = 'S') |> List.head |> fst 
let endpos = map |> Map.toList |> List.filter (fun (_,c) -> c = 'E') |> List.head |> fst

let rec shortest (cost:int) (available:Set<Pos>) (visited: Set<Pos>) (frontier:Pos list) (goal:Pos) =
    if frontier.Contains goal then cost
    else
        let cost = cost + 1
        let visited = Set.union visited (frontier |> Set.ofList)
        let frontier = frontier |> List.map neighbours |> List.concat  
                       |> List.filter (fun p -> available.Contains p)
                       |> List.filter (fun p -> visited.Contains p |> not)
        shortest cost available visited frontier goal 

let solve (map:Map<Pos,char>) (shortcut:Pos) =
    printf "."
    let available = map |> Map.toList
                    |> List.filter (fun (_,c) -> c <> '#')
                    |> List.map fst |> Set.ofList
    let available = available.Add shortcut 
    let visited = Set.empty
    let frontier = [startpos]
    shortest 0 available visited frontier endpos


let canBeSkipped (map:Map<Pos,char>) (pos:Pos) =
    let validChar c = c = Some '.' || c = Some 'E' || c = Some 'S'
    neighbours pos
    |> List.filter (fun p -> map.TryFind p |> validChar)
    |> List.length > 1
    
let shortcuts = map |> Map.toList
                |> List.filter (fun (p,c) -> c = '#')
                |> List.map fst
                |> List.filter (canBeSkipped map) 
let not_cheating = solve map (-5,-5)
printfn $"SHORTEST: {not_cheating}"

printfn $"shortcuts: {shortcuts.Length}"

let skiptimes = shortcuts
                |> List.map (fun shortcut -> solve map shortcut)
                |> List.map (fun i -> not_cheating - i)
                |> List.filter (fun i -> i > 0)
                
let summary = skiptimes |> List.groupBy id |> List.map (fun (i,il) -> il.Length,i)

printfn $"Summary {summary}"

let limit = 100
let answer = summary |> List.filter (fun (c,i) -> i >= limit) |> List.map fst |> List.sum 

printfn $"ANSWER 1: {answer}"

