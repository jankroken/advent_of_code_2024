open System.IO

type INT = System.Numerics.BigInteger
let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "/users/Jan/Downloads/input-nav.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> List.map (printfn "%A")

let towels =
    input[0].Split(",")
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.map (_.ToCharArray())
    |> List.map Array.toList

towels |> print

let patterns = input.Tail.Tail |> List.map _.ToCharArray() |> List.map Array.toList

patterns |> printlist

printfn "----"

type Towel = char list

let rec possible (impossibles:Set<int>) (towels: Towel list) (pattern: char list) : Set<int>*Option<Towel List> =
    printfn $"Possible: impossible={impossibles} {pattern}"

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
        printfn "pattern is matched"
        impossibles,Some []
    elif impossibles.Contains pattern.Length then impossibles,None 
    else
        let candidates = towels |> List.map (fun t -> tryMatch pattern t t) |> List.choose id
        // printfn  "Candidates:"
        // candidates |> printlist

        let rec tryAll (impossibles: Set<int>) (candidates: (Towel*char list) list) : Set<int>*Option<Towel list> =
            // printfn $"Trying: {candidates}"

            if candidates.IsEmpty then
                impossibles, None 
            else
                let impossibles, firstMatch = possible impossibles towels (snd candidates.Head)

                if firstMatch = None then
                    // printfn "No match"
                    let impossibles = impossibles.Add (snd candidates.Head).Length
                    tryAll impossibles candidates.Tail
                else
                    printfn $"Match {fst candidates.Head}"
                    impossibles, firstMatch |> Option.map (fun towels -> (fst candidates.Head :: towels) )
        let impossibles, result = tryAll impossibles candidates
        impossibles, result 
            
let towelMatch = possible Set.empty towels (patterns[0])

printfn $"TOWEL MATCH: {towelMatch}"

// possible towels ['r']

let answer1 = patterns |> List.map (possible Set.empty towels) |> List.map snd |> List.choose id |> List.length

// printfn "TOWEL S: "
// answer1 |> printlist 

printfn $"ANSWER 1: {answer1}"

// patterns[5] |> printfn "WEIRD: %A"

// printfn "-------"

// patterns[5] |> possible towels