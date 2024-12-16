open System.IO

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> printlist 

let parse (s: string list) =
    let s = s |> List.rev
    let xy (y:int,(s:string)) =
        let s = s.ToCharArray()
        s |> Array.toList |> List.indexed |> List.map (fun (x,c) -> (x,y),c)
    let s = s |> List.indexed |> List.map xy |> List.concat 
    s |> Map.ofList

type Pos = int*int 
type Maze = Map<Pos,char>

let printMap (map:Maze) =
    let map = map |> Map.toList
    let map = map |> List.groupBy (fun ((_,y),_) -> y)
    let map = map |> List.sort |> List.rev |> List.map snd 
    let printline (l:(Pos*char) list) =
        let l = l |> List.sort |> List.map snd |> List.map (fun c -> $"{c}")
        System.String.Concat(l) |> printfn "%s"
    map |> List.map printline 
    
let mapOrig : Maze = parse input
let start : Pos = mapOrig |> Map.toList |> List.find (fun (_,v) -> v = 'S') |> fst
let goal : Pos = mapOrig |> Map.toList |> List.find (fun (_,v) -> v = 'E') |> fst

printfn $"start->goal: {start} -> {goal}"

let neighs ((x,y):Pos) =
    [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    
let deadEnd (map:Maze) (pos:Pos) : bool =
    if map.TryFind pos = Some('.') |> not then false
    else
        let blocked = neighs pos |> List.map (fun n -> map.TryFind n = Some('#') || map.TryFind n = Some('*')) |> List.filter id |> List.length
        blocked >= 3
        
let removeDeadEnds (map:Maze) =
    let rec markDeads (map:Maze) (deads:Pos list) =
        if deads = [] then map
        else
            markDeads (map.Add (deads.Head, '#')) deads.Tail

    let rec remove (map:Maze) =
        let tiles = map |> Map.toList |> List.filter (fun (_,v) -> v = '.') |> List.map fst
        let deadEnds = tiles |> List.filter (deadEnd map)
        let map = markDeads map deadEnds 
        printfn $"DEAD ENDS: {deadEnds}"
        if deadEnds.Length > 0 then
            remove map
        else 
            map 
    remove map

let map = removeDeadEnds mapOrig

type Dir = N | S | W | E

type Cache = Map<Pos*Dir,int64>

let initCache = Map.empty.Add ((start,E),0L)

let rotLeft (dir:Dir) =
    match dir with
    | N -> W
    | W -> S
    | S -> E
    | E -> N
let rotRight (dir:Dir) =
    match dir with 
    | N -> E
    | E -> S
    | S -> W
    | W -> N

let from (((x,y),dir): Pos*Dir) : Pos=
    match dir with
    | N -> (x,y-1)
    | S -> (x,y+1)
    | E -> (x-1,y)
    | W -> (x+1,y)

let newCost (map:Maze) (cache:Cache) ((pos,dir):Pos*Dir) =
    let curr = cache.TryFind (pos, dir)
    let rr = cache.TryFind (pos, rotRight dir) |> Option.map (fun v -> v + 1000L)
    let rl = cache.TryFind (pos, rotLeft dir) |> Option.map (fun v -> v + 1000L)
    let move = cache.TryFind (from (pos,dir),dir) |> Option.map (fun v -> v + 1L)
    let options = [rr;rl;move] |> List.choose id
    let options = if curr.IsSome then options |> List.filter (fun v -> v < curr.Value) else options
    let res = if options.IsEmpty then None else Some (options |> List.min)
    // if res.IsSome then printfn $"newCost ({pos},{dir} options={options} -> {res}"
    res
    
newCost map initCache ((1,2),N)    
newCost map initCache ((2,1),E)

let traversable =
    map |> Map.toList |> List.filter (fun (_,v) -> v <> '#') |> List.map fst
    |> List.map (fun pos -> [(pos,N);(pos,S);(pos,E);(pos,W)])
    |> List.concat

let rec updateCache (cache:Cache) (values:((Pos*Dir)*int64) list) =
    if values = [] then cache
    else updateCache (cache.Add values.Head) values.Tail 

let rec solve (limit:int) (i:int) (cache:Cache) =
    // cache |> printfn "Cache: %A"
    let newCosts = traversable
                   |> List.map (fun pd -> pd,(newCost map cache pd))
                   |> List.filter (fun (pd,v) -> v.IsSome)
                   |> List.map (fun (pd,v) -> (pd,v.Value))
    // printfn $"newCosts: {newCosts}"
    let cache = updateCache cache newCosts 
    if newCosts.IsEmpty then // || i > limit then
        // if not newCosts.IsEmpty then printfn "Terminating early"
        cache
    else
        solve limit (i+1) cache 
let finalCache = solve 100000 0 initCache
printfn $"Final cache: {finalCache}"

let answer1 = finalCache
              |> Map.toList |> List.filter (fun ((p,d),v) -> p = goal)
              |> List.map snd
              |> List.min 


printfn $"ANSWER 1: {answer1}"

let canComeFrom (cache:Cache) (here:(Pos*Dir)) =
    let currValue = cache[here]
    let from = from here
    let r = fst here, snd here |> rotLeft
    let l = fst here, snd here |> rotRight
    let fromX = cache.TryFind (from,snd here) |> Option.filter (fun v -> v = currValue - 1L) |> Option.map (fun _ -> (from,snd here))
    let rX = cache.TryFind r |> Option.filter (fun v -> v = currValue - 1000L) |> Option.map (fun _ -> r)
    let lX = cache.TryFind l |> Option.filter (fun v -> v = currValue - 1000L) |> Option.map (fun _ -> l)
    [fromX;rX;lX] |> List.choose id

let rec bestSeats (seats:Set<Pos>) (frontier:(Pos*Dir) list) (cache:Cache) =
    let seats = Set.union seats (frontier |> List.map fst |> Set.ofList)
    printfn $"bestSeats: {seats} {frontier} [cache]"
    if frontier.IsEmpty then seats
    else
        let newFrontier = frontier |> List.map (canComeFrom cache) |> List.concat
        newFrontier |> List.map (printfn "new frontier: %A")
        bestSeats seats newFrontier cache

let goals = finalCache
            |> Map.toList |> List.filter (fun ((p,_),_) -> p = goal)
            |> List.filter (fun (_,v) -> v = answer1)|> List.map fst

let seats = bestSeats Set.empty goals finalCache

seats.Count |> printfn "ANSWER 2: %A"
