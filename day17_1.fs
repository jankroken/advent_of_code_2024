open System.IO

let print a = a |> printfn "%A"
let printlist a = a |> List.map print
let inputFile = "input.txt"
let input = File.ReadAllLines inputFile |> Array.toList

input |> printlist

let powOf2 (n: int64) =
    let rec pow (p: int64) (n: int64) =
        if n = 0 then p else pow (p * 2L) (n - 1L)

    pow 1L n


type Computer(program: int64 list, a: int64, b: int64, c: int64, ip: int64, output: int64 list) as self =
    let program = program
    let a = a
    let b = b
    let c = c
    let ip = ip
    let output = output

    member this.A = a
    member this.B = b
    member this.C = c
    member this.Program = program
    member this.Ip = ip
    member this.Output = output

    member this.ComboValue(operand: int64) : int64 =
        match operand with
        | c when c >= 0 && c <= 3 -> c
        | c when c = 4 -> this.A
        | c when c = 5 -> this.B
        | c when c = 6 -> this.C
        | c -> failwith $"Combo operator: {c}"

    member this.EvalInstruction(inst: int64, operand: int64) : Computer =
        match inst with
        | 0L -> // adv (division) A = A/2^(operand) |> truncate
            let a = this.A / powOf2 (operand)
            Computer(program, a, this.B, this.C, this.Ip + 2L, this.Output)
        | 1L -> // bxl (b = b XOR operand)
            let value = this.B ^^^ operand
            Computer(program, this.A, value, this.C, this.Ip + 2L, this.Output)
        | 2L -> // bst (b = combo(oper) % 8)
            let value = this.ComboValue(operand) % 8L
            Computer(program, this.A, value, this.C, this.Ip + 2L, this.Output)
        | 3L -> // jnz -> if A <> 0 then ip = operand
            let ip = if this.A = 0L then this.Ip + 2L else operand
            Computer(program, this.A, this.B, this.C, ip, this.Output)
        | 4L -> // bxc B = B xor C (ignores operand)
            let value = this.B ^^^ this.C
            Computer(program, this.A, value, this.C, this.Ip + 2L, this.Output)
        | 5L -> // out combo(operand) % 8 |> print
            let output = this.ComboValue(operand) % 8L
            Computer(program, this.A, this.B, this.C, this.Ip + 2L, output :: this.Output)
        | 6L -> // bdv B = A/2^combo(operand) |> truncate
            let value = this.A / powOf2 (this.ComboValue(operand))
            Computer(program, this.A, value, this.C, this.Ip + 2L, this.Output)
        | 7L -> // cdv C = A/2^combo(operand) |> truncate
            let value = this.A / powOf2 (this.ComboValue(operand))
            Computer(program, this.A, this.B, value, this.Ip + 2L, this.Output)
        | i -> failwith $"Unknown instruction: {i}"

    member this.Exec() =
        if ip |> int >= program.Length - 1 then
            // printfn $"Halting at ip = {ip}"
            this
        else
            let i = program[ip |> int]
            let o = program[(ip |> int) + 1]
            this.EvalInstruction(i, o).Exec()

    member this.GetOutput() : int64 list = this.Output |> List.rev

    member this.setA(value: int64) =
        Computer(program, value, this.B, this.C, this.Ip, this.Output)


    override this.ToString() =
        $"Computer(A={this.A},B={this.B},C={this.C} ip={this.Ip} output = {this.Output} program={this.Program})"

let parse (input: string list) =
    let parseR (s: string) =
        let s = (s.Split(" "))
        let name = (s[1].Split(":"))[0]
        let value = s[2] |> int64
        (name, value)

    let separator = input |> List.tryFindIndex (fun c -> c = "")
    printfn $"separator = {separator}"
    let registers = input |> List.take separator.Value |> List.map parseR
    let program = input |> List.last

    let program =
        program.Split(" ")
        |> (fun c -> c.[1])
        |> (fun c -> c.Split(","))
        |> Array.toList
        |> List.map int64

    printfn "registers:"
    registers |> printlist
    printfn "program:"
    program |> print
    registers, program
    Computer(program, registers[0] |> snd, registers[1] |> snd, registers[2] |> snd, 0L, [])

let computer = parse input

computer |> print

powOf2 (2) |> print

computer.EvalInstruction(0L, 2L) |> print

let computerX = Computer([ 0L; 1L; 5L; 4L; 3L; 0L ], 2024L, 2024L, 43690L, 0L, [])
computerX.Exec() |> print

computer.Exec().GetOutput()
|> List.map (fun c -> $"{c}")
|> String.concat ","
|> printfn "ANSWER 1: %A"

print "N: 1,6,3,6,5,6,5,1,7"

let search (comp: Computer) =
    let computer = computer.setA (0L)
    let solved (computer: Computer) = computer.GetOutput() = computer.Program

    let rec solve (n: int64) =
        if (n % 10000L = 0L) then
            printfn $"trying {n}"

        if computer.Exec() |> solved then n else solve (n + 1L)

    let answer2 = solve 0L
    printfn $"ANSWER 2: {answer2}"

// search computer

computer.Program

let printInst (i: int64) (o: int64) =
    let combo (i: int64) =
        match i with
        | c when c < 4L -> $"{c}"
        | 4L -> "A"
        | 5L -> "B"
        | 6L -> "C"

    let lit (i: int64) = $"{i}"

    match i with
    | 0L -> $"A = A / 2^{combo o}"
    | 1L -> $"B = B XOR {lit o}"
    | 2L -> $"B = B %% 8 (& 111)"
    | 3L -> $"IF A <> 0 GOTO {lit o}"
    | 4L -> "B = B XOR C"
    | 5L -> $"OUT {combo o} %% 8"
    | 6L -> $"B = A / 2^{combo o}"
    | 7L -> $"C = A / 2^{combo o}"

let i = printInst computer.Program[0] computer.Program[1]
print i

let rec writeable (program:int64 list) =
    match program with
    | [] -> []
    | [a] -> [$"{a}"]
    | a::b::rest ->
        let inst = printInst a b
        let rest = writeable (b::rest)
        inst::rest

let asm = writeable computer.Program

printfn "PROGRAM (ASM): "
asm |> printlist

let rec block (program:string list) : string list =
    match program with
    | [] -> []
    | [a] -> [a]
    | a::_::rest -> a :: (block rest)
    
let block0 = block asm
let block1 = block asm.Tail

printfn "0:"
block0 |> List.map (printfn "%s")
printfn "1:"
block1 |> List.map (printfn "%s")

print asm.Length

let program = computer.Program |> List.map (int)
let xor6 = program |> List.map (fun c -> c ^^^ 6)

let printProgram (p:int list) =
    p |> List.map (fun c -> $"{c}") |> String.concat " " |> printfn "%s"

printfn "XOR 6:" 
xor6 |> printProgram 
