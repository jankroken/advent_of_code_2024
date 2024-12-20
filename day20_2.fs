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

let rec addAll (map:Map<Pos,int>) (value:int) (poss:Pos list) =
    if poss.IsEmpty then map
    else addAll (map.Add (poss.Head,value)) value poss.Tail 

let rec shortest (cost:int) (available:Set<Pos>) (visited: Map<Pos,int>) (frontier:Pos list) (goal:Pos) =
    if frontier.IsEmpty then visited 
    else
        let visited = addAll visited cost frontier
        let cost = cost + 1
        let frontier = frontier |> List.map neighbours |> List.concat  
                       |> List.filter (fun p -> available.Contains p)
                       |> List.filter (fun p -> visited.ContainsKey p |> not)
        shortest cost available visited frontier goal 

let solve (map:Map<Pos,char>) (from:Pos) (goal:Pos)=
    printf "."
    let available = map |> Map.toList
                    |> List.filter (fun (_,c) -> c <> '#')
                    |> List.map fst |> Set.ofList
    let visited = Map.empty
    let frontier = [from]
    shortest 0 available visited frontier goal

let forwards = solve map startpos endpos
let backwards = solve map endpos startpos

printfn $"FORWARDS: from:{forwards[startpos]} to:{forwards[endpos]} map:{forwards}"
printfn $"BACKWARDS: from:{backwards[endpos]} to:{backwards[startpos]} map:{backwards}"

let makeCross (n:int) =
    let y = seq {-n .. n }
    let x (y:int) : Pos list =
        let n = n - (abs y)
        seq { -n .. n } |> Seq.map (fun x -> x,y) |> Seq.toList 
    y |> Seq.map x |> Seq.toList |> List.concat 

let cross = makeCross 20 |> List.filter (fun i -> i <> (0,0))

cross |> printfn "CROSS: %A"

let findShortcuts (limit:int) (bestscore:int) =
    let endCandidates = backwards.Keys |> Seq.toList
    let eval (endpoint:Pos) =
        let distanceToEnd = backwards.TryFind endpoint
        if distanceToEnd = None || distanceToEnd.Value > (bestscore - limit) then
            // printfn $"BAD ENDPOINT: {endpoint} to-end:{distanceToEnd}"
            [] // either not an endpoint, or already too far from the end
        else
            let evalCrossPos (cpos:Pos) =
                let cost = (abs (fst cpos)) + (abs (snd cpos))
                let start = (fst cpos) + (fst endpoint), (snd cpos) + (snd endpoint)
                let cs = forwards.TryFind start
                if cs.IsNone then 0
                else
                    let totalCost = cs.Value + cost + distanceToEnd.Value
                    let saving = bestscore - totalCost 
                    if saving < limit then 0
                    else 
                        // printfn $"Avail.Skip: {start} to {endpoint} cost: {cs}->{cost}->{distanceToEnd}={totalCost}"
                        bestscore - totalCost 
            cross |> List.map evalCrossPos |> List.filter (fun i -> i > 0)
    endCandidates |> List.map eval |> List.concat 
    
let bestCost = forwards[endpos]

let shortcuts =
    findShortcuts 100 bestCost |> List.groupBy id |> List.map (fun (v,l) -> v, l.Length)
    |> List.sortBy fst

let sum = shortcuts |> List.map snd |> List.sum 

shortcuts |> List.map (fun sc -> printfn $"Saves {fst sc} : {snd sc}")

printfn $"ANSWER 2: {sum}"
