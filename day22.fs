open System.IO
open System.Linq

type INT = System.Numerics.BigInteger

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList |> List.map INT.Parse

input |> printlist




let evolve (secret: INT) =
    let secret = (secret ^^^ (secret * 64I)) % 16777216I
    let secret = (secret ^^^ (secret / 32I)) % 16777216I
    let secret = (secret ^^^ (secret * 2048I)) % 16777216I
    secret

printfn "------"
42I ^^^ 15I |> print
100000000I % 16777216I |> print
evolve 123I |> print

let rec evolveN (times: int) (secret: INT) =
    if times = 0 then
        secret
    else
        let secret = evolve secret
        evolveN (times - 1) secret

1I |> evolveN 2000 |> print

input |> List.map (evolveN 2000) |> List.sum |> printfn "Answer 1: %A"

let ones2 (secret: INT) =
    let rec ones (acc: INT) (secret: INT) =
        if secret = 0I then
            acc
        else
            let digit = secret % 2I
            let secret = secret / 2I
            // let acc = if digit = 1I then acc * 2I + 1I else acc
            let acc = acc + digit 
            ones acc secret

    ones 0I secret

let ones n = n % 10I

ones 15887950I |> ones |> printfn "ONES: %A"

type Sequence = INT*INT*INT*INT

type PriceHistory(count: int, price: INT, p1d: INT, p2d: INT, p3d: INT, p4d: INT) =
    member this.AddPrice(secret: INT) =
        let newPrice = ones secret
        // printfn $"p5d = {newPrice} - {price}"
        let p5d = newPrice - price 
        PriceHistory (count + 1, newPrice, p2d, p3d, p4d, p5d)

    member this.Sequence = p1d, p2d, p3d, p4d
    member this.Price = price
    member this.Valid = count > 4
    
    override this.ToString() =
        if count > 4 then 
            $"PriceHistory([{p1d} {p2d} {p3d} {p4d} -> {price})"
        else
            $"PriceHistory(INVALID)"

type SeqMap = Map<INT*INT*INT*INT,INT>

let rec search (map:SeqMap) (times:int) (priceHistory:PriceHistory) (secret: INT) =
    if times = 0 then
        map
    else
        let priceHistory = priceHistory.AddPrice secret
        let updateMap = priceHistory.Valid && (map.ContainsKey priceHistory.Sequence |> not)
        let map = if updateMap then map.Add (priceHistory.Sequence,priceHistory.Price) else map 
        // printfn $"{priceHistory}"
        let secret = evolve secret
        search map (times - 1) priceHistory secret
        
let findMap (seed:INT) =
    printf "."
    search Map.empty 2001 (PriceHistory(0,0L,0L,0L,0L,0L)) seed

// findMap 123I |> Map.toList |> List.map (printfn "%A")

printf "Finding maps for each key: "
let maps = input |> List.map findMap

printfn ""

printf "Finding keys: "

let keys = maps |> List.map (fun map -> printf "." ; map.Keys |> Set.ofSeq) |> Set.unionMany

printfn ""

let value key = maps |> List.map (fun map -> map.TryFind key) |> List.choose id |> List.sum

printf "Finding values: "
let keyvalues = keys |> Set.toList |> List.map (fun key -> printf "."; key,value key)

// keyvalues |> List.map print 
printfn ""
printf "Finding best price: "
let bestPrice = keyvalues |> List.map snd |> List.max

printfn $"{bestPrice}"

let bestSequence = keyvalues |> List.filter (fun (_,v) -> v = bestPrice)

printfn $"bestSequence: {bestSequence}"

printfn $"keys.Size = {keys.Count}"

