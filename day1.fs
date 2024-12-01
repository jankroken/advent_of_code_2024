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

// let ints = l |> List.map (fun l -> l |> List.map toInt)

let sums (l:int64 list) = l |> List.sum 

// printfn $"{ints |> List.map sums }"

let split (s:string) : int*int =
    let s = s.Split " "
    printfn $"{s.Length}"
    (s[0] |> int,s[3] |> int)
    
// input |> List.map split |> List.map (fun s -> printfn $"{s}")

let pairs = input |> List.map (fun s -> s |> split)

let firsts = pairs |> List.map fst |> List.sort 
let seconds = pairs |> List.map snd  |> List.sort

let npairs = List.zip firsts seconds

let dist (p:int*int):int =
    (fst p - snd p) |> abs 

npairs |> List.map dist |> List.sum  |> (fun s -> printfn $"SUM: {s}")

let sims (l: int list) (n:int) : int =
    l |> List.filter (fun i -> i = n) |> List.length |> (fun i -> i*n)
    
let sims1 = sims seconds

let allsims = firsts |> List.map sims1

// allsims |> List.map (printfn "%A")

allsims |> List.sum |> printfn "SCORE: %A" 
