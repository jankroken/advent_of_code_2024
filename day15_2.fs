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
// printlist input

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

let expandLine (l: string) =
    let expandChar (c: char) =
        match c with
        | '#' -> "##"
        | '.' -> ".."
        | '@' -> "@."
        | 'O' -> "[]"

    l.ToCharArray() |> Array.toList |> List.map expandChar |> System.String.Concat

let map: Map<Pos, char> = fst inputs |> List.map expandLine |> toMap
let dirs = snd inputs

printfn $"MAP {map}"
printfn $"DIRS {dirs}"

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
    let leftside= map[x,y] = '['
    let rightside = map[x,y] = ']'
    let safe (pos: Pos) =
        match map.TryFind(pos) with
        | Some('.') -> true
        | Some('[') -> true
        | Some(']') -> true
        | Some('@') -> true
        | _ -> false

    let u = (x, y + 1) |> safe
    let d = (x, y - 1) |> safe
    let l = (x - 1, y) |> safe
    let r = (x + 1, y) |> safe
    ((u && d) || (l && r)) |> not

let allBoxes (map: Map<Pos, char>) =
    map |> Map.toList |> List.filter (fun (_, v) -> v = '[' || v = ']') |> List.map fst

let allLocked (map: Map<Pos, char>) =
    map |> allBoxes |> List.filter (locked map)

let setAllTo (c: char) (map: Map<Pos, char>) (poss: Set<Pos>) =
    map |> Map.map (fun k v -> if poss.Contains k then c else v)

let tidyMap1 (map: Map<Pos, char>) =
    let lockedBoxes = allLocked map |> Set.ofList
    let lefts = lockedBoxes |> Set.filter (fun c -> map[c] = '[')
    let rights = lockedBoxes |> Set.filter (fun c -> lefts.Contains c |> not)
    let map = setAllTo '(' map lefts
    setAllTo ')' map rights

let tidyMap (map:Map<Pos, char>) =
    map |> tidyMap1 |> tidyMap1 

let withRobot (map: Map<Pos, char>) (robot: Pos) = map.Add(robot, '@')

let rmRobot (map: Map<Pos, char>) (robot: Pos) = map.Add(robot, '.')

let setupMap (map: Map<Pos, char>) : Map<Pos, char> * Pos =
    printfn "Setup:"
    printMap map
    let robot = findRobot map
    let map = rmRobot map robot
    let map: Map<Pos, char> = tidyMap map
    printfn "Tidy:"
    printMap (withRobot map robot)
    map, robot

let rec pushd (map: Map<Pos, char>) (acc:Pos list) ((dx, dy): Pos) ((x, y): Pos) : Option<Pos*(Pos list)> =
    let next = (x + dx, y + dy)

    match map.TryFind next with
    | Some('.') -> Some (next,acc)
    | Some(']') -> pushd map (next::acc) (dx, dy) next
    | Some('[') -> pushd map (next::acc) (dx, dy) next
    | _ -> None

let delta (dirchar: char) =
    match dirchar with
    | '<' -> (-1, 0)
    | '>' -> (1, 0)
    | '^' -> (0, 1)
    | 'v' -> (0, -1)

let push (map: Map<Pos, char>) (robot: Pos) (dir: char) =
    let delta = delta dir
    pushd map [] delta robot

let moveRobotPos (dir: char) ((x, y): Pos) =
    let (dx, dy) = delta dir
    (x + dx, y + dy)

let flipr (c:char) = if c = '[' then ']' elif c = ']' then '[' else c

let flipAll (map:Map<Pos,char>) (poss:Set<Pos>) =
    map |> Map.map (fun k v -> if poss.Contains k && (v = '[' || v = ']') then flipr v else v)

let moveHorizontally (map: Map<Pos, char>) (robot: Pos) (dir: char) =
    printfn $"MOVE {robot} {dir}"
    let newRobotPos = moveRobotPos dir robot
    let nextTile = map.TryFind newRobotPos

    match nextTile with
    | Some('#') -> map, robot
    | Some(')') -> map, robot
    | Some('(') -> map, robot
    | Some('.') -> map, newRobotPos
    | Some(rp) when rp = ']' || rp = '[' ->
        printfn "PUSHING!"
        let filled = push map robot dir

        match filled with
        | Some (newBox,moved) when (dir = '<' || dir = '>') ->
            let map = map.Add(newBox, if dir = '<' then '[' else ']')
            let map = flipAll map (moved |> Set.ofList)
            printfn $"moved:{moved}"
            let map = map.Add(newRobotPos, '.')
            map, newRobotPos
        | None -> map,robot

let addXY ((x,y):Pos) ((dx,dy):Pos) = (x+dx),(y+dy)

let rec boxesAt (map:Map<Pos,char>) (poss:(Pos*char) list) : Pos list =
    match poss with
    | [] -> []
    | a::rest when map[fst a] = '.' -> boxesAt map rest
    | ((x,y),_)::rest when map[x,y] = ']' -> (x-1,y)::(x,y)::(boxesAt map rest)
    | ((x,y),_)::rest when map[x,y] = '[' -> (x+1,y)::(x,y)::(boxesAt map rest)
    
let hasBlockers (map:Map<Pos,char>) (poss:(Pos*char) list) =
    poss |> List.exists (fun (p,_) -> map[p] = '#' || map[p] = '(' || map[p] = ')')

let rec pushV (remove:Pos list) (add:(Pos*char) list) (map: Map<Pos, char>) (dir:char) (moving: Pos list) : Option<Pos list*(Pos*char) list> =
    // printfn $"pushV moving:{moving} {delta dir}"
    let above : (Pos*char) list = moving |> List.map (fun p -> (addXY p (delta dir)),map[p])
    let remove = List.concat [remove;moving]
    let add = List.concat [add;above]
    // printfn $"pushV above={above}"
    // printfn $"      remove={remove}"
    // printfn $"      add={add}"
    // printfn $"      hasblockers={hasBlockers map above}"
    if hasBlockers map above then None
    elif moving = [] then Some(remove,add)
    else 
        let boxesAbove = boxesAt map above
        pushV remove add map dir boxesAbove
        
let rec addAll (map:Map<Pos,char>) (entries:(Pos*char) list) =
    if entries = [] then map
    else
        let map = map.Add entries.Head
        addAll map entries.Tail 
 
let pushVert (map:Map<Pos,char>) (robot:Pos) (dir: char) =
    pushV [] [] map dir [robot]
    
let moveVertically (map: Map<Pos, char>) (robot: Pos) (dir: char) =
    let newRobotPos = moveRobotPos dir robot
    printfn $"MOVE VERT {robot} {dir} -> {newRobotPos}"
    let nextTile = map.TryFind newRobotPos

    match nextTile with
    | Some('#') -> map, robot
    | Some('(') -> map, robot
    | Some(')') -> map, robot
    | Some('.') -> map, newRobotPos
    | Some(rp) when rp = ']' || rp = '[' ->
        printfn "PUSHING VERT!"
        let filled = pushVert map robot dir

        match filled with
        | Some (toRemove,toAdd) when (dir = '^' || dir = 'v') ->
            let toRemove = toRemove |> List.map (fun p -> p,'.')
            let map = addAll map toRemove 
            let map = addAll map toAdd
            let map = map.Add(newRobotPos, '.')
            map, newRobotPos
        | None ->
            map,robot 

let rec solve (limit:int) (steps: int) (map: Map<Pos, char>) (robot: Pos) (dirs: char list) =
    let dir = dirs.Head
    let dirs = dirs.Tail
    let map = if (steps % 10) = 0 then tidyMap map else map
    let horizontalMove = dir = '<' || dir = '>'
    
    let map, robot =
        if horizontalMove then 
            moveHorizontally map robot dir
        else
            moveVertically map robot dir
    // printfn "AFTER: "
    // printMap (withRobot map robot)
    if dirs = [] then // || steps > limit then
        map, robot
    else
        solve limit (steps + 1) map robot dirs

let score (map: Map<Pos, char>) =
    let maxY = map |> Map.keys |> Seq.max |> snd
    let yv y = maxY - y
    let score (x, y) = 100 * (yv y) + x
    printfn $"SCORE (4,8) = {score (4, 8)}"

    map
    |> Map.toList
    |> List.filter (fun (_, v) -> v = '[' || v = '(')
    |> List.map fst
    |> List.map score
    |> List.sum


let part1 () =
    // let limit = 2000
    let map, robot = setupMap map
    let map, robot = solve -1 0 map robot dirs
    let score = score map
    withRobot map robot |> printMap 
    printfn $"ANSWER 1: {score}"

part1 ()
