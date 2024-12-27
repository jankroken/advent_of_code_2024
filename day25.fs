open System.IO
open System.Linq

type INT = System.Numerics.BigInteger

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "/users/Jan/Downloads/input-irc.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> printlist

let rec split (acc:string list list) (curr:string list) (input: string list) : string list list =
    match input,curr with
    | [],[] -> acc |> List.rev
    | [],curr ->
        let acc = (curr |> List.rev) :: acc
        split acc [] []
    | ""::rest,[] ->
        split acc [] rest
    | ""::rest,curr ->
        let acc = (curr |> List.rev) :: acc
        split acc [] rest 
    | line::rest,curr ->
        let curr = line::curr
        split acc curr rest
        
let splitBlock (block: string list) : Set<int*int> =
    let block = block |> List.indexed
    let toXY (y:int,line:string) =
        line.ToCharArray() |> Array.toList |> List.indexed |> List.map (fun (x,c) -> (x,y),c)
    block |> List.map toXY |> List.concat |> Map.ofList
    |> Map.filter (fun _ v -> v = '#')
    |> Map.keys |> Set.ofSeq 



let isLock (block:Set<int*int>) = block.Contains ((0,0))

type Type = Lock | Key 

let heights (block:Set<int*int>) =
    let isLock = isLock block
    let lockHeights y = y
    let keyHeight y = 6 - y
    let t = if isLock then Lock else Key 
    let xs = block |> Set.map fst
    let height (x:int) =
        block
        |> Set.filter (fun (bx,_) -> bx = x)
        |> Set.map snd
        |> if isLock then
            (fun s -> s |> Set.maxElement |> lockHeights)
            else (fun s -> s |> Set.minElement |> keyHeight)
    t,xs |> Set.toList |> List.map height
    
let locksAndKeys = input |> split [] [] |> List.map splitBlock  |> List.map heights
// locksAndKeys |> List.map print

let keys = locksAndKeys |> List.filter (fun (t,_) -> t = Key) |> List.map snd 
let locks = locksAndKeys |> List.filter (fun (t,_) -> t = Lock) |> List.map snd 

keys
|> List.map (fun key -> locks |> List.map (fun lock -> List.zip key lock))
|> List.concat
|> List.map (fun lockAndKey -> lockAndKey |> List.map (fun (l,k) -> l+k))
|> List.map List.max
|> List.filter (fun i -> i <= 5)
|> List.length
|> printfn "Answer: %A"


