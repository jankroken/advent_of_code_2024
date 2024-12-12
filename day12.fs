open System
open System.IO

let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> List.map (printfn "%A")

type Pos = int * int

let toSet (input: string list) : Map<Pos, char> =
    let input = input |> List.rev

    let toXY (y: int, line: string) =
        line.ToCharArray()
        |> Array.toList
        |> List.indexed
        |> List.map (fun (x, c) -> (x, y), c)

    input |> List.indexed |> List.map toXY |> List.concat |> Map.ofList

let map = input |> toSet

let neighs ((x, y): Pos) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let rec expand (visited: Set<Pos>) (pos: Pos) : Set<Pos> =
    let n = neighs pos
    let value = map[pos]

    let n =
        neighs pos
        |> List.filter (fun n -> visited.Contains n |> not)
        |> List.filter (fun p -> map.TryFind p = Some(value))

    if n = [] then
        visited
    else
        let visited = Set.union visited (n |> Set.ofList)
        n |> List.map (expand visited) |> Set.unionMany

let findArea2 (map: Map<Pos, char>) : Set<Pos> =
    if map.IsEmpty then
        Set.empty
    else
        let start = map.Keys |> Seq.head
        printfn $"{start} -> {map[start]}"
        expand (Set.empty.Add start) start

let rec expand2 (visited: Set<Pos>) (frontier: Pos list) : Set<Pos> =
    let value = map[frontier.Head]
    let visited = Set.union visited (frontier |> Set.ofList)

    let frontier =
        frontier
        |> List.map neighs
        |> List.concat
        |> List.distinct
        |> List.filter (fun p -> visited.Contains p |> not)
        |> List.filter (fun n -> map.TryFind n = Some(value))

    if frontier.IsEmpty then
        visited
    else
        expand2 visited frontier

let findArea (map: Map<Pos, char>) : Set<Pos> =
    if map.IsEmpty then
        Set.empty
    else
        let start = map.Keys |> Seq.head
        printfn $"{start} -> {map[start]}"
        expand2 Set.empty [ start ]

let area = findArea map

printfn $"AREA {area}"

let removeArea (area: Set<Pos>) (map: Map<Pos, char>) =
    map |> Map.filter (fun p v -> (area.Contains p |> not))

let rec splitIntoAreas (map: Map<Pos, char>) : Set<Pos> list =
    if map.IsEmpty then
        []
    else
        let area = findArea map
        let map = removeArea area map
        area :: (splitIntoAreas map)

let areas = splitIntoAreas map

areas |> List.map (printfn "AREA %A")

let perimeter (area: Set<Pos>) : int =
    area
    |> Set.toList
    |> List.map neighs
    |> List.map (List.filter (fun p -> (area.Contains p |> not)))
    |> List.map List.length
    |> List.sum

let price (area: Set<Pos>) =
    let peri = perimeter area
    let area = area.Count
    peri * area

let totalPrice = areas |> List.map price |> List.sum

totalPrice |> printfn "ANSWER 1: %A"

let sides (area: Set<Pos>) : int =
    let aboves ((x, y): Pos) =
        let above = (x, y + 1)
        let above = (area.Contains above |> not)
        if above then [ (x, y + 1) ] else []

    let belows ((x, y): Pos) =
        let below = (x, y - 1)
        let below = (area.Contains below |> not)
        if below then [ (x, y) ] else []

    let befores ((x, y): Pos) =
        let before = (x - 1, y)
        let before = (area.Contains before |> not)
        if before then [ (x, y) ] else []

    let afters ((x, y): Pos) =
        let after = (x + 1, y)
        let after = (area.Contains after |> not)
        if after then [ (x + 1, y) ] else []

    let aboves = area |> Set.toList |> List.map aboves |> List.concat
    let belows = area |> Set.toList |> List.map belows |> List.concat
    let befores = area |> Set.toList |> List.map befores |> List.concat
    let afters = area |> Set.toList |> List.map afters |> List.concat

    let aboves =
        aboves
        |> List.groupBy (fun (_, y) -> y)
        |> List.map snd
        |> List.map (List.map fst)

    let belows =
        belows
        |> List.groupBy (fun (_, y) -> y)
        |> List.map snd
        |> List.map (List.map fst)


    let befores =
        befores
        |> List.groupBy (fun (x, _) -> x)
        |> List.map snd
        |> List.map (List.map snd)

    let afters =
        afters
        |> List.groupBy (fun (x, _) -> x)
        |> List.map snd
        |> List.map (List.map snd)


    let rec continuous (l: int list) =
        match l with
        | [] -> 0
        | a :: b :: rest when b = a + 1 -> continuous (b :: rest)
        | _ :: rest -> 1 + (continuous rest)

    let befores = befores |> List.map continuous
    let afters = afters |> List.map continuous
    let aboves = aboves |> List.map continuous
    let belows = belows |> List.map continuous
    [ befores; afters; aboves; belows ] |> List.concat |> List.sum

areas |> List.map sides |> List.sum |> printfn "SIDES: %A"

let discountPrice (area: Set<Pos>) = area.Count * (sides area)

areas |> List.map discountPrice |> List.sum |> printfn "%A"
