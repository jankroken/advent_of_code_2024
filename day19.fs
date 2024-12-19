open System.IO

type INT = System.Numerics.BigInteger
let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

// input |> List.map (printfn "%A")

let towels =
    input[0].Split(",")
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.map (_.ToCharArray())
    |> List.map Array.toList

let patterns = input.Tail.Tail |> List.map _.ToCharArray() |> List.map Array.toList

type Towel = char list
type Cache = Map<int,INT>

let rec possible (cache:Cache) (towels: Towel list) (pattern: char list) : Cache*INT =
    let rec tryMatch (pattern: char list) (orig: Towel) (towel: Towel) : Option<Towel*char list> =
        if towel.IsEmpty then
            Some (orig,pattern)
        elif pattern.IsEmpty then
            None
        elif towel.Head = pattern.Head then
            tryMatch pattern.Tail orig towel.Tail
        else
            None

    if pattern = [] then
        cache.Add (0,1I),1I
    elif cache.ContainsKey pattern.Length then cache,cache[pattern.Length]
    else
        let candidates = towels |> List.map (fun t -> tryMatch pattern t t) |> List.choose id

        let rec tryAll (curr:INT) (cache: Cache) (candidates: (Towel*char list) list) : Cache*INT =

            if candidates.IsEmpty then
                cache, curr
            else
                let cache, firstMatch = possible cache towels (snd candidates.Head)

                if firstMatch = 0I then
                    let cache = cache.Add ((snd candidates.Head).Length,0I)
                    tryAll curr cache candidates.Tail
                else
                    let cache = cache.Add ((snd candidates.Head).Length,firstMatch)
                    tryAll (curr+firstMatch) cache candidates.Tail
        let cache, result = tryAll 0I cache candidates
        cache.Add (pattern.Length, result), result 
            
let answers = patterns |> List.map (possible Map.empty towels) |> List.map snd

let answer1 = answers |> List.filter (fun i -> i > 0L) |> List.length
let answer2 = answers |> List.sum 

printfn $"ANSWER 1: {answer1}"
printfn $"ANSWER 2: {answer2}"
