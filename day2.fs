open System.IO 

let inputFile = "input.txt"

let input = File.ReadAllLines inputFile |> Seq.toList 

let toInt (s:string) : int64 = s |> int64 

let rec splitOnLine (splitOn: 'T->bool) (lines: 'T list) : 'T list list =
   let rec accumulate (completed: 'T list list) (curr: 'T list) (rem: 'T list) : 'T list list =
         match completed,curr,rem with
         | _,_,a::rest when splitOn a ->
             accumulate ((curr |> List.rev)::completed) [] rest
         | _,_,a::rest ->
             accumulate completed (a::curr) rest
         | _,[],[] ->
             completed |> List.rev
         | _,_,[] ->
             (curr |> List.rev) :: completed |> List.rev
         | _,_,_ -> failwith $"Not handled: compl={completed} curr={curr} rest={rem}"
   accumulate [] [] lines

let l = input |> splitOnLine (fun t -> t = "")

let split (s:string) : int64 list = 
    let s = s.Split " " |> Array.toList |> List.map (fun s -> s |> int64)
    printfn $"{s.Length}"
    s
    
let mess = input |> List.map split

mess |> List.map (printfn "%A")

let rec safe (inc:bool) (l:int64 list) =
    match l with
    | [] -> true
    | a::[] -> true
    | a::b::_ when inc && b < a -> false 
    | a::b::_ when not inc && b > a -> false
    | a::b::rest ->
        let diff = a-b |> abs
        match diff with
        | 1L -> safe inc (b::rest)
        | 2L -> safe inc (b::rest)
        | 3L -> safe inc (b::rest)
        | _ -> false 
let safe1 (l:int64 list) : bool =
    safe true l || safe false l 

let safes = mess |> List.filter safe1

printfn $"SAFE: {safes |> List.length}"

let rec safer (inc:bool) (l:int64 list)=
    let ok a b =
        let dir = if inc then b > a else a > b
        let dist = a - b |> abs
        dir && dist > 0L && dist < 4L
    match l with
    | [] -> true
    | [_] -> true
    | [_;_] -> true
    | a::b::c::rest when ok a b && ok b c -> safer inc (b::c::rest)
    | a::b::c::rest ->
        safe inc (a::b::rest) || safe inc (a::c::rest)

let safe2 (l:int64 list) : bool =
    safer true l || safer false l || safe1 (List.tail l)

let safes2 = mess |> List.filter safe2

printfn $"SAFE2: {safes2 |> List.length}"
