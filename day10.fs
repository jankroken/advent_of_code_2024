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

type Pos = int*int 

let toMap (s:string list) =
    let s = s |> List.rev
    let rec toList (acc: (Pos*int) list list) (y:int) (s:string list) =
        match s with
        | [] -> acc |> List.concat 
        | xs::rest ->
            let xs = xs.ToCharArray() |> Array.toList |> List.indexed
                     |> List.map (fun (x,c) -> ((x,y),$"{c}" |> int))
            let acc = xs :: acc
            toList acc (y+1) rest
    toList [] 0 s |> Map.ofList 

let map = toMap input

let max_X,max_Y = map.Keys |> Seq.max

let neighbours (x:int,y:int) : Pos list = [x+1,y;x-1,y;x,y+1;x,y-1]

let starts = map |> Map.toList |> List.filter (fun (p,c) -> c = 0) |> List.map fst
            // |> List.filter (fun (x,y) -> x = 0 || x = max_X || y = 0 || y = max_Y)

starts |> List.map (printfn "START: %A")

let next (map:Map<Pos,int>) (pos:Pos) =
    let height = map[pos]
    neighbours pos |> List.filter (fun p -> map.TryFind p = Some(height+1))

starts.Head |> next map |> printfn "NEXT: %A"

let rec find9s (poss:Pos list) =
    if poss = [] then 0L
    elif map[poss.Head] = 9 then poss.Length
    else
        let poss = poss |> List.map (next map) |> List.concat |> Set.ofList |> Set.toList
        find9s poss

let rec find9s_2 (poss:Pos list) =
    if poss = [] then 0L
    elif map[poss.Head] = 9 then poss.Length
    else
        let poss = poss |> List.map (next map) |> List.concat
        find9s_2 poss 

starts |> List.map (fun p -> find9s [p]) |> List.sum |> printfn "ANSWER 1: %A" 
starts |> List.map (fun p -> find9s_2 [p]) |> List.sum |> printfn "ANSWER 2: %A" 
