open System.IO
open System.Linq

type INT = System.Numerics.BigInteger

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "/users/Jan/Downloads/input-nav.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> printlist

let initLines, ruleLines =
    let i = input |> List.findIndex (fun line -> line = "")
    let initLines = input |> List.take i
    let rules = input |> List.skip (i + 1)
    initLines |> List.map (printfn "INIT: %A")
    rules |> List.map (printfn "RULE: %A")
    initLines, rules

let inits =
    initLines
    |> List.map (_.Split(":"))
    |> List.map (fun kv -> kv[0], kv[1].Trim() |> INT.Parse)
    |> Map.ofList

inits |> print

type OP =
    | AND
    | OR
    | XOR

type Rule = OP * string * string * string

let rules: Rule list =
    let parseLine (line: string) =
        let line = line.Split(" ")

        match line[1] with
        | "AND" -> AND, line[0], line[2], line[4]
        | "OR" -> OR, line[0], line[2], line[4]
        | "XOR" -> XOR, line[0], line[2], line[4]

    ruleLines |> List.map parseLine

let rec addAll (map: Map<'a, 'b>) (l: ('a * 'b) list) =
    if l.IsEmpty then map else addAll (map.Add l.Head) l.Tail

let rec solve (known: Map<string, INT>) (rules: Rule list) =
    let canBeResolved ((_, reg1, reg2, _): Rule) =
        known.ContainsKey reg1 && known.ContainsKey reg2

    let unsolvable rule = canBeResolved rule |> not

    let resolve ((op, reg1, reg2, res): Rule) =
        match op with
        | AND ->
            if known[reg1] = 1I && known[reg2] = 1I then
                (res, 1I)
            else
                (res, 0I)
        | OR ->
            if known[reg1] = 1I || known[reg2] = 1I then
                (res, 1I)
            else
                (res, 0I)
        | XOR -> if known[reg1] = known[reg2] then (res, 0I) else (res, 1I)

    let solutions = rules |> List.filter canBeResolved |> List.map resolve
    let known = addAll known solutions
    let notSolved = rules |> List.filter unsolvable
    if notSolved.IsEmpty then known else solve known notSolved

let solution = solve inits rules
solution |> Map.toList |> print

let binary =
    solution
    |> Map.toList
    |> List.filter (fun (k, v) -> k.StartsWith "z")
    |> List.sort
    |> List.map snd

let toDecimal (binary: INT list) =
    let binary = binary |> List.rev

    let rec toDecimal (acc: INT) (binary: INT list) =
        if binary.IsEmpty then
            acc
        else
            toDecimal (acc * 2I + binary.Head) binary.Tail

    toDecimal 0I binary

let answer1 = binary |> toDecimal
printfn $"Answer 1: {answer1}"

type Bitlet = string * string * string

let bits =
    let zs = rules |> List.map (fun (_, _, _, z) -> z)
    let highestBit = zs |> List.max |> _.Substring(1) |> int

    let toBitlet n : Bitlet =
        let x = n |> sprintf "x%02d"
        let y = n |> sprintf "y%02d"
        let z = n |> sprintf "z%02d"
        Bitlet(x, y, z)

    seq { 0..highestBit } |> Seq.toList

type Register(i: int) =
    let i = i
    let x = i |> sprintf "x%02d"
    let prevX = i - 1 |> sprintf "x%02d"

    let y = i |> sprintf "y%02d"
    let prevY = i - 1 |> sprintf "y%02d"

    let z = i |> sprintf "z%02d"
    let prevZ = i |> sprintf "z%02d"

    member this.I = i
    member this.X = x
    member this.PrevX = prevX
    member this.Y = y
    member this.PrevY = prevY
    member this.Z = z
    member this.PrevZ = prevZ

    override this.ToString() = $"Registers({i})"

let registers = bits |> List.map Register
let fourthOf4 (_, _, _, c) = c

let check0 (reg: Register) =
    let rulesXY =
        rules
        |> List.filter (fun (op, x, y, z) -> op = XOR && x = reg.X && y = reg.Y && z = reg.Z)

    let rulesYX =
        rules
        |> List.filter (fun (op, y, x, z) -> op = XOR && x = reg.X && y = reg.Y && z = reg.Z)

    let xor_x_y_z = [ rulesXY; rulesYX ] |> List.concat

    let rulesXY =
        rules |> List.filter (fun (op, x, y, _) -> op = AND && x = reg.X && y = reg.Y)

    let rulesYX =
        rules |> List.filter (fun (op, y, x, _) -> op = AND && x = reg.X && y = reg.Y)

    let carry = [ rulesXY; rulesYX ] |> List.concat

    if xor_x_y_z.Length <> 1 then
        printfn $"check0: Wrong count for XOR(X,Y)->Z : {xor_x_y_z}"
        None
    elif carry.Length <> 1 then
        printfn $"check0: Wrong count for carry: {carry}"
        None
    else
        printfn $"check0 passed: xor:{xor_x_y_z} carry:{carry}"
        Some(carry.Head |> fourthOf4)

check0 registers[0] |> printfn "check0: %A"

let checkN (reg: Register) (carry: string) =
    printfn $"checkN {reg} carry:{carry}"
    let tmpZ_XY =
        rules
        |> List.filter (fun (op, x, y, z) -> op = XOR && x = reg.X && y = reg.Y)

    let tmpZ_YX =
        rules
        |> List.filter (fun (op, y, x, z) -> op = XOR && x = reg.X && y = reg.Y)

    let tmpZ = [ tmpZ_XY; tmpZ_YX ] |> List.concat

    if tmpZ.Length <> 1 then
        printfn $"tmpZ: wrong length: {tmpZ}"
        None
    else
        let tmpZ = tmpZ.Head |> fourthOf4
        let z_XY =
            rules
            |> List.filter (fun (op, x, y, z) -> op = XOR && x = tmpZ && y = carry && z = reg.Z)

        let z_YX =
            rules
            |> List.filter (fun (op, y, x, z) -> op = XOR && x = tmpZ && y = carry && z = reg.Z)
       
        let z = [ z_XY; z_YX ] |> List.concat
        
        let carry_XY =
            rules
            |> List.filter (fun (op, x, y, z) -> op = AND && x = tmpZ && y = carry)

        let carry_YX =
            rules
            |> List.filter (fun (op, y, x, z) -> op = AND && x = tmpZ && y = carry)

        let carry = [ carry_XY; carry_YX ] |> List.concat
        if z.Length <> 1 then
            printfn $"Z: wrong length: {z}"
            None
        elif carry.Length <> 1 then
            printfn $"carry: wrong length: {carry}"
            None
        else
            let z = z.Head
            let carry = carry.Head |> fourthOf4
            printfn $"[{reg.I}] carry: {carry}"
            Some (carry)
            
checkN registers[1] "wrn"