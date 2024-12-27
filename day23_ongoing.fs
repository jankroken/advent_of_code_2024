open System.IO
open System.Linq

type INT = System.Numerics.BigInteger

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> printlist

let parseLine (s:string) = s.Split("-") |> Set.ofArray

type Lan = Set<string>
type Link = Set<string>
type Node = string
let trueSet = Set.empty.Add true
let link (a:Node) (b:Node) : Link = [a;b] |> Set.ofList 

let links : Link list = input |> List.map parseLine

let nodes : Set<Node> = links |> Set.unionMany

let tnodes = nodes |> Set.filter (fun s -> s.StartsWith "t")

// tnodes |> Set.map (printfn "%A")

let combine (l:'a list) : Set<'a> list =
    let rec combine (l:'a list) : Set<'a> list list =
        match l with
        | [] -> []
        | a::rest ->
            // printfn $"combining a: {a}"
            (rest |> List.map (fun b -> [a;b] |> Set.ofList)) :: (combine rest)
    combine l |> List.concat  
    
combine [1;2;3;4] |> print 


let findTriangles (node:string) =
    let connectedTo = links |> List.filter (fun s -> s.Contains node)
                      |> List.map (fun s -> s |> Set.filter (fun v -> v <> node))
                      |> List.map (_.MinimumElement)
    let links = Set.ofList links
    let triangles = connectedTo
                    |> combine
                    |> List.filter (links.Contains)
                    |> List.map (fun s -> s.Add node)
    triangles 
                     
let triangles = tnodes |> Set.toList |> List.map findTriangles |> List.concat |> Set.ofList 

printfn $"ANSWER 1: {triangles.Count}"

let neighbourMap : Map<Node,Set<Node>> =
    let sndlist (kvl: (Node*Node) list) : Node list = kvl |> List.map snd 
    links |> List.map Set.toList
          |> List.map (fun [a;b] -> [a,b;b,a])
          |> List.concat
          |> List.filter (fun (a,b) -> a <> b)
          |> List.groupBy fst
          |> List.map (fun (k,kvl) -> k,(kvl |> sndlist |> Set.ofList))
          |> Map.ofList 

let rec expandLan (largestLan:int) (lan:Lan) : Set<Lan> =
    let candidates = nodes |> Set.filter (fun n -> lan.Contains n |> not)
    let candidates = candidates |> Set.filter (fun n -> (Set.intersect neighbourMap[n] lan) = lan)
    let possibleSize = lan.Count + candidates.Count
//    printfn $"expandLan: lan: {lan.Count} candidates: {candidates.Count} largest lan: {largestLan} possible:{possibleSize}"
    if possibleSize < largestLan then Set.empty 
    else candidates |> Set.map lan.Add

let solveForLan (largestLan: int) (lan:Lan) =
    let expandLans (lans: Set<Lan>) =
        lans
        |> Set.map (expandLan largestLan)
        |> Set.unionMany
    let rec solve (count:int) (lans:Set<Lan>) =
        let newLans = expandLans lans
        // printfn $"Expanded ({count}): newLans: {newLans.Count}"
        if newLans.Count = 0 then
            lans.MinimumElement
        elif newLans.Count = 1 then
            newLans.MinimumElement
        else
            solve (count+1) newLans 
    solve 0 (Set.empty.Add lan)

let rec solve (largestLan:Lan) (triangles:Set<Lan>) =
    printfn $"Solve largestLan: {largestLan.Count} triangles: {triangles.Count}"
    if triangles.IsEmpty then largestLan
    else 
        let lan = triangles.MinimumElement
        let rest = triangles.Remove lan
        let expandedLan = solveForLan (largestLan.Count) lan
        let largestLan = if expandedLan.Count > largestLan.Count then expandedLan else largestLan
        solve largestLan rest
    
let largest = solve Set.empty triangles
largest |> Set.toList |> List.sort |> String.concat "," |> printfn "Answer 2: %A"
        
