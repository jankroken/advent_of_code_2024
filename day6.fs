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

let toMap (sl:string list) =
    let rec toMap (y:int) (sl:string list) : ((int*int)*char) list list =
        match sl with
        | [] -> []
        | s::sl ->
            let value1 = s |> Seq.toList |> List.indexed |> List.map (fun (x,c) -> (x,y),c)
            value1 :: (toMap (y+1) sl)
    toMap 0 sl |> List.concat |> Map.ofList 
            
let mapWithGuard = input  |> toMap

type Dir =
    | NORTH
    | EAST
    | SOUTH
    | WEST

let rotate (dir:Dir) : Dir =
    match dir with
    | NORTH -> EAST
    | EAST -> SOUTH
    | SOUTH -> WEST
    | WEST -> NORTH
    
type Pos = int*int

let move ((x,y):Pos) (dir:Dir) : Pos =
    match dir with
    | NORTH -> (x,y-1)
    | EAST -> (x+1,y)
    | SOUTH -> (x,y+1)
    | WEST -> (x-1,y)

let guardInitPos = mapWithGuard |> Map.toList |> List.filter (fun (_,v) -> v = '^') |> List.head |> fst

guardInitPos |> printfn "%A"

let map = mapWithGuard.Add (guardInitPos,'.')

let blocked (pos:Pos) = map.TryFind pos = Some('#')
let outside (pos:Pos) = map.TryFind pos = None 

let rec walk (visited:Set<Pos>) (pos:Pos) (dir:Dir) : Set<Pos> =
//    printfn $"WALK {visited} {pos} {dir}"
    let next = move pos dir
    if outside next then
//        printfn $"OUTSIDE {next}"
        visited
    elif blocked next then
//        printfn $"BLOCKED {next}"
        walk visited pos (rotate dir)
    else
        let visited = visited.Add next
        walk visited next dir

let steps = walk (Set.empty.Add (guardInitPos)) guardInitPos NORTH

printfn $"ANSWER 1: {steps.Count}"

let rec isLoop (map:Map<Pos,char>) (visited:Set<Pos*Dir>) (pos:Pos) (dir:Dir) : bool =
    let blocked (pos:Pos) = map.TryFind pos = Some('#')
    let outside (pos:Pos) = map.TryFind pos = None 
    let next = move pos dir
    if outside next then
        // printfn $"OUTSIDE {next}"
        false
    elif visited.Contains (next,dir) then
        // printfn $"LOOP {(next,dir)}"
        true
    elif blocked next then
        // printfn $"BLOCKED {next}"
        isLoop map visited pos (rotate dir)
    else
        let visited = visited.Add (next,dir)
        isLoop map visited next dir

printfn $"guardInitPos {guardInitPos}"        
    
let option1 = map.Add ((3,6),'#')

let initVisited = Set.empty.Add (guardInitPos,NORTH)

let initIsLoop = isLoop map initVisited guardInitPos NORTH 
let o1IsLoop = isLoop option1 initVisited guardInitPos NORTH 

printfn $"loops: INIT {initIsLoop} {o1IsLoop}"

let makeOption (pos:Pos) = map.Add (pos,'#')
let options = steps |> Set.remove guardInitPos |> Set.toList
              |> List.map (fun pos -> (map.Add (pos,'#')))

let mapIsLoop (map:Map<Pos,char>) =
    printf "."
    isLoop map initVisited guardInitPos NORTH 
     
options |> List.map mapIsLoop |> List.filter id |> List.length |> printfn "ANSWER 2: %A"
