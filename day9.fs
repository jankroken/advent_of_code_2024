open System
open System.IO
open System.Security.Cryptography.X509Certificates

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

// input |> List.map (printfn "%A")


let bytes = input[0].ToCharArray() |> Array.toList
          |> List.map (fun c -> $"{c}" |> int64)

let toFS (bytes:int64 list) =
  let rec toFS (rfs: (int64*int64*int64) list) (fn: int64) (bytes: int64 list) =
    match bytes with
    | [c] ->
        let rfs = (fn,c,0L) :: rfs 
        rfs |> List.rev 
    | a::b::bytes ->
        let rfs = (fn,a,b)::rfs 
        let fn = fn + 1L 
        toFS rfs fn bytes
  toFS [] 0L bytes 

let cfs = bytes |> toFS
bytes |> printfn "%A"
cfs |> fun s -> printfn $"{s.Length} : {s}"

type Block = int64*int64*int64

let compress (cfs:Block list) =
    let rec compress (rfs:Block list) (cfs:Block list) =
        match cfs with
        | [fn,fs,_] -> (fn,fs,0L) :: rfs |> List.rev
        | [fn1,bs1,0L;fn2,bs2,_] when fn1 = fn2 ->
            (fn1,bs1+bs2,0L) :: rfs |> List.rev 
        | (_,_,0L)::rest ->
            let rfs = cfs.Head :: rfs
            let cfs = cfs.Tail
            compress rfs cfs
        | (fn1,bs1,free1)::rest ->
            match cfs |> List.last with
            | (fn2,bs2,_) when bs2 <= free1 ->
                let rfs = (fn1,bs1,0L) :: rfs
                let cfs = cfs.Tail 
                let cfs = cfs |> List.removeAt ((cfs |> List.length) - 1)
                let cfs = (fn2,bs2,free1-bs2)::cfs
                compress rfs cfs
            | (fn2,bs2,_) when bs2 > free1 ->
                let rfs = (fn1,bs1,0L) :: rfs
                let cfs = cfs.Tail 
                let cfs = cfs |> List.removeAt ((cfs |> List.length) - 1)
                let cfs = cfs |> List.insertAt (cfs |> List.length) (fn2,bs2-free1,0L)
                let cfs = (fn2,free1,0L) :: cfs
                compress rfs cfs
    compress [] cfs 
                
let ex1 = compress [0L,1L,2L;1L,3L,4L;2L,5L,0L]
            
ex1 |> printfn "%A"

let fs2 = "2333133121414131402".ToCharArray() |> Array.toList
          |> List.map (fun c -> $"{c}") |> List.map int64 |> toFS
          
let ex2 = compress fs2

ex2 |> printfn "%A"

let rec checksum (i:int64) (cs:int64) (blocks:Block list) =
    match blocks with
    | [] -> cs
    | (_,0L,0L)::rest -> checksum i cs rest
    | (a,0L,free)::rest ->
        let blocks = (a,0L,free-1L) :: rest
        checksum (i+1L) cs blocks 
    | (fn,bs,free)::rest ->
        let cs = cs + (fn*i)
        let i = i + 1L
        let blocks = (fn,bs-1L,free)::rest
        checksum i cs blocks 
checksum 0L 0L ex2  |> printfn "%A"

cfs |> compress |> checksum 0L 0L |> printfn "ANSWER 1 %A"

let compress2 (blocks:Block list) =
    let blocks = blocks |> List.rev
    let rec compress2 (cfs:Block list) (blocks:Block list) =
        match blocks with
        | [] -> cfs
        | [first] -> first::cfs
        | (fn2,bs2,free2)::(fn1,bs1,free1)::before ->
            let canBeMovedFarAhead = before |> List.exists (fun (_,_,free) -> free >= bs2)
            let canBeShifted = free1 >= bs2
            if canBeMovedFarAhead then
                let target = before |> List.findIndexBack (fun (_,_,free) -> free >= bs2)
                let (fnt,bst,tfree) = before[target]
                let before = before |> List.removeAt target
                let before = List.insertAt target (fnt,bst,0L) before 
                let before = List.insertAt target (fn2,bs2,tfree-bs2) before
                let blocks = (fn1,bs1,free1+bs2+free2) :: before  
                // printfn $"TARGET: {target} {before |> List.rev}"
                compress2 cfs blocks 
            elif canBeShifted then
                let cfs = (fn2,bs2,free1+free2) :: cfs
                let blocks = (fn1,bs1,0L)::before
                // printfn $"SHIFTING: {fn2} -> {fn1}"
                compress2 cfs blocks
            else
                let cfs = blocks.Head :: cfs
                let blocks = blocks.Tail
                // printfn $"SKIPPING: {fn2}"
                compress2 cfs blocks 
    compress2 [] blocks
        

let e2 = fs2 |> compress2

// printfn $"E2 = {e2}"

// e2 |> List.map (printfn "E2 BLOCK: %A")

e2 |> checksum 0L 0L |> printfn "%A"
                
cfs |> compress2 |> checksum 0L 0L |> printfn "ANSWER2: %A"              
   
