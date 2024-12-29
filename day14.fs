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
let width = if test then 11 else 101
let height = if test then 7 else 103
let input = File.ReadAllLines inputFile |> Array.toList

let middleX = (width - 1) / 2

input |> printlist

type Pos = int * int
type Vel = int * int
type Robot = Pos * Vel

let parse (s: string) : Robot =
    let s = s.Split(" ")
    let pos = s[0].Split("=")
    let pos = pos[1].Split(",")
    let pos = (pos[0] |> int, pos[1] |> int)
    let vel = (s[1].Split("=")[1]).Split(",")
    let vel = (vel[0] |> int, vel[1] |> int)
    pos, vel

let robots = input |> List.map parse

robots |> printlistf "ROBOT %A"

let moverobot ((((x, y): Pos), ((dx, dy): Vel)): Robot) : Robot =
    let (x, y), vel = (x + dx, y + dy), (dx, dy)
    let x = (x + width) % width
    let y = (y + height) % height
    (x, y), vel

let tick (robots: Robot list) : Robot list = robots |> List.map moverobot

let rec tickn (n: int) (robots: Robot list) : Robot list =
    if n = 0 then robots else robots |> tick |> tickn (n - 1)

let robots100 = robots |> tickn 100

let quadrant1 =
    robots100
    |> List.filter (fun ((x, y), _) -> x < ((width - 1) / 2) && y < ((height - 1) / 2))

let quadrant2 =
    robots100
    |> List.filter (fun ((x, y), _) -> x > ((width - 1) / 2) && y < ((height - 1) / 2))

let quadrant3 =
    robots100
    |> List.filter (fun ((x, y), _) -> x < ((width - 1) / 2) && y > ((height - 1) / 2))

let quadrant4 =
    robots100
    |> List.filter (fun ((x, y), _) -> x > ((width - 1) / 2) && y > ((height - 1) / 2))

let safe (quad: Robot list) = quad |> List.length

let quads =
    [ quadrant1; quadrant2; quadrant3; quadrant4 ]
    |> List.map safe
    |> List.map int64

quads |> printfn "%A"
let safety = quads[0] * quads[1] * quads[2] * quads[3]

printfn $"ANSWER 1 {safety}"

let example: Robot = (2, 4), (2, -3)

let rec movern (n: int) (robot: Robot) =
    if n = 0 then
        true
    else
        printfn $"ROBOT @ {n} : {robot}"
        moverobot robot |> movern (n - 1)

// movern 6 example

let printrobots (robots: Set<Pos>) =
    let xs = seq { 0 .. width - 1 } |> Seq.toList
    let ys = seq { 0 .. height - 1 } |> Seq.toList

    let charAt (pos) =
        if robots.Contains pos then "#" else "."

    let toString y =
        let chars = xs |> List.map (fun x -> charAt (x, y)) |> List.toArray
        System.String.Concat(chars)

    ys |> List.map toString |> List.map (printfn "%s")

let wrap (x, y) =
    if x > (width - 1) / 2 then (width - x - 1, y) else (x, y)

let possibleTree (robots: Robot list) =
    let robots = robots |> List.map fst |> Set.ofList
    let midX = (width - 1)

    if robots.Contains (midX, 0) |> not then
        false
    else
        let nostem = robots |> Set.filter (fun (x, y) -> x <> (width - 1) / 2)
        let wrapped = nostem |> Set.map wrap

        if wrapped.Count * 2 <> nostem.Count then
            false
        else
            printfn "Possible:"
            printrobots robots
            true

wrap (50, 0) |> printfn "WRAPPED: %A"
(width - 1) / 2 |> printfn "MID: %A"

let robots2 =
    [ ((50, 0), (0, 0)); ((49, 1), (0, 0)); ((50, 1), (0, 0)); ((51, 1), (0, 0)) ]

// possibleTree robots2


let neighbours ((x,y):Pos) =
    [x-1,y
     x+1,y
     x,y-1
     x,y+1]
let quickCheck1 (robots:Robot list) =
    let dots =
        robots |> List.map fst
        |> Set.ofList
    let allneighbours (dot:Robot) : bool =
        let dot = fst dot 
        let neighbours = neighbours dot
        neighbours |> List.filter dots.Contains |> List.length = 4
    robots |> Seq.ofList |> Seq.filter allneighbours |> Seq.length > 3 
        
let rec findTree (count: int) (robots: Robot list) =
    if count % 10000 = 0 then
        printfn $"@{count}"
        // robots |> List.map fst |> Set.ofList |> printrobots
        ()
    if quickCheck1 robots then
        printfn $"POSSIBLE: {count}"
        robots |> List.map fst |> Set.ofList |> printrobots
    else
        let robots = robots |> List.map moverobot
        let limit = count + 1
        findTree limit robots

findTree 0 robots


let findPos((tx,ty):Pos) (((x,y),(dx,dy)): Robot) : int64*int64 =
    printfn $"robot: {tx},{ty} <- {x},{y} , {dx},{dy}"
    
    (0,0)
    
    

findPos (0,50) robots[1]

robots |> List.filter (fun (_,(a,b)) -> a = 0 or b = 0 ) |> printlist 


robots.Length |> print
robots |> List.max |> print 
