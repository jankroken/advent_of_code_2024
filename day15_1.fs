open System.IO

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let printlistf f a = a |> List.map (printfn f)

let test = false

let inputFile =
    if test then
        "input-test.txt"
    else
        "input.txt"

let input = File.ReadAllLines inputFile |> Array.toList
printlist input

type Pos = int * int

let toMap (input: string list) =
    let toXYC (y: int, line: string) =
        let line = line.ToCharArray() |> Array.toList |> List.indexed
        line |> List.map (fun (x, c) -> (x, y), c)

    let input = input |> List.rev
    input |> List.indexed |> List.map toXYC |> List.concat |> Map.ofList

let bifurcate (input) =
    let rec split maplines input =
        match input with
        | "" :: rest -> (List.rev maplines), rest
        | m :: rest -> split (m :: maplines) rest

    let maplines, dirlines = split [] input
    let dirlines = dirlines |> List.map (_.ToCharArray()) |> List.map (Array.toList)
    let dirs = dirlines |> List.concat
    maplines, dirs

let inputs = bifurcate input
let map: Map<Pos, char> = fst inputs |> toMap
let dirs = snd inputs

printfn $"MAP {map}"
printfn $"DIRS {dirs}"

let fixedBoxes: Set<Pos> = Set.empty

let findRobot (map: Map<Pos, char>) =
    map |> Map.toList |> List.find (fun (_, v) -> v = '@') |> fst

let robot = findRobot map

printfn $"ROBOT {robot}"

let printMap (map: Map<Pos, char>) =
    let map =
        map
        |> Map.toList
        |> List.groupBy (fun ((_, y), _) -> y)
        |> List.sort
        |> List.rev

    let map = map |> List.map snd
    let map = map |> List.map (List.map snd)

    map
    |> List.map (fun line ->
        line |> List.map (fun c -> printf $"{c}")
        printfn "")

let locked (map: Map<Pos, char>) ((x, y): Pos) : bool =
    let safe (pos: Pos) =
        match map.TryFind(pos) with
        | Some('.') -> true
        | Some('O') -> true
        | Some('@') -> true
        | _ -> false

    let u = (x, y + 1) |> safe
    let d = (x, y - 1) |> safe
    let l = (x - 1, y) |> safe
    let r = (x + 1, y) |> safe
    ((u && d) || (l && r)) |> not

let allBoxes (map: Map<Pos, char>) =
    map |> Map.toList |> List.filter (fun (_, v) -> v = 'O') |> List.map fst

let allLocked (map: Map<Pos, char>) =
    map |> allBoxes |> List.filter (locked map)

let setAllTo (c: char) (map: Map<Pos, char>) (poss: Set<Pos>) =
    map |> Map.map (fun k v -> if poss.Contains k then c else v)

let tidyMap (map: Map<Pos, char>) =
    let lockedBoxes = allLocked map |> Set.ofList
    setAllTo 'X' map lockedBoxes

let withRobot (map: Map<Pos, char>) (robot: Pos) = map.Add(robot, '@')

let rmRobot (map: Map<Pos, char>) (robot: Pos) = map.Add(robot, '.')

let setupMap (map: Map<Pos, char>) : Map<Pos,char>*Pos=
    printfn "Setup:"
    printMap map
    let robot = findRobot map
    let map = rmRobot map robot 
    let map : Map<Pos,char> = tidyMap map
    printfn "Tidy:"
    printMap map
    map,robot 

let rec pushd (map:Map<Pos,char>) ((dx,dy):Pos)  ((x,y):Pos) =
    let next = (x+dx,y+dy)
    match map.TryFind next with
    | Some('.') -> Some next
    | Some('O') -> pushd map (dx,dy) next
    | _ -> None 

let delta (dirchar:char) =
    match dirchar with
    | '<' -> (-1,0)
    | '>' -> (1,0)
    | '^' -> (0,1)
    | 'v' -> (0,-1)

let push (map:Map<Pos,char>) (robot:Pos) (dir:char) =
    let delta = delta dir
    pushd map delta robot 

let moveRobotPos (dir:char) ((x,y):Pos) =
    let (dx,dy) = delta dir 
    (x+dx,y+dy)

let move (map:Map<Pos,char>) (robot:Pos) (dir:char) =
    printfn $"MOVE {robot} {dir}"
    let newRobotPos = moveRobotPos dir robot
    let nextTile = map.TryFind newRobotPos
    match nextTile with
    | Some('#') -> map,robot
    | Some('X') -> map,robot
    | Some('.') -> map,newRobotPos
    | Some('O') ->
        printfn "PUSHING!"
        let filled = push map robot dir
        match filled with
        | Some newBox ->
            let map = map.Add (newBox, 'O')
            let map = map.Add (newRobotPos,'.')
            map,newRobotPos
        | None ->
            map,robot

let rec solve (steps:int) (map:Map<Pos,char>) (robot:Pos) (dirs:char list) =
    let dir = dirs.Head
    let dirs = dirs.Tail
    let map = if (steps % 10) = 0 then tidyMap map else map 

    let map,robot = move map robot dir
    // printfn "AFTER: "
    // printMap (withRobot map robot)
    if dirs = [] then map,robot
    else solve (steps + 1) map robot dirs

let score (map:Map<Pos,char>) =
    let maxY = map |> Map.keys |> Seq.max |> snd 
    let yv y = maxY - y
    let score (x,y) = 100*(yv y)+x
    printfn $"SCORE (4,8) = {score (4,8)}"
    map |> Map.toList
    |> List.filter (fun (_,v) -> v = 'O' || v = 'X')
    |> List.map fst |> List.map score
    |> List.sum
    

let part1 () =
    let map,robot = setupMap map
    let map,robot = solve 0 map robot dirs
    let score = score map
    printfn $"ANSWER 1: {score}"

part1 ()
