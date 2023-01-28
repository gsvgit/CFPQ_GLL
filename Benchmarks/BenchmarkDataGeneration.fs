module Benchmarks.BenchmarkDataGeneration

open System.Text.RegularExpressions

let private seed = 100
let private rnd = System.Random(seed)

type private SyntaxError =
    | KeywordError
    | BracketError
    | OperationError
    | DotCommaError
    | TruncatedBlockError
    | TruncatedLineError
    with
        static member toInt (e: SyntaxError) =
            match e with
            | KeywordError -> 1
            | BracketError -> 2
            | OperationError -> 3
            | DotCommaError -> 4
            | TruncatedBlockError -> 5
            | TruncatedLineError -> 6
        static member fromInt (i: int) =
            match i with
            | 1 -> KeywordError
            | 2 -> BracketError
            | 3 -> OperationError
            | 4 -> DotCommaError
            | 5 -> TruncatedBlockError
            | 6 -> TruncatedLineError
            | _ -> failwith "Invalid SyntaxError"

let private keywords = [| "if"; "else"; "true"; "false"; "return"; "bool"; "int"; "func"|]
let private brackets = [| "\("; "\)"; "\{"; "\}" |]
let private operations = [| "\+"; "-"; "\*"; "\^"; "="; "<"; ">"; "<="; ">="; "!="; "&&"; "||" |]

let private breakKeyword (keyword: string) =
    let index = rnd.Next(0, keyword.Length)
    match rnd.Next 2 with
    | 0 -> keyword.Substring(0, index) + "x" + keyword.Substring(index)
    | 1 -> keyword.Substring(0, index) + " " + keyword.Substring(index, keyword.Length - index)
    | _ -> failwith "Invalid random number"

let private mkError (program: string) (e: SyntaxError) =
    let lines = program.Split "\n"

    let mkErrorByPattern (patterns: string array) (breaker: string -> string) =
        let pattern = patterns.[rnd.Next(0, patterns.Length)]
        let regex = Regex pattern
        regex.Replace(program, breaker pattern, 1)

    let mkErrorByRemove patterns = mkErrorByPattern patterns (fun _ -> "")

    match e with
    | KeywordError -> mkErrorByPattern keywords breakKeyword
    | BracketError -> mkErrorByRemove brackets
    | OperationError -> mkErrorByRemove operations
    | DotCommaError -> mkErrorByRemove [| ";" |]
    | TruncatedBlockError ->
        let indexFrom = rnd.Next(0, lines.Length)
        let indexTo = rnd.Next(indexFrom, lines.Length)
        let newLines = Array.concat [ lines[0..indexFrom]; lines[indexTo..] ]
        String.concat "\n" newLines
    | TruncatedLineError ->
        let index = rnd.Next(0, lines.Length)
        let line = lines.[index]
        let truncateIndex = rnd.Next(0, line.Length)
        lines[index] <- line.Substring(0, truncateIndex)
        String.concat "\n" lines



let private samples = [| Tests.GolangRSM.functionSample; Tests.GolangRSM.cycleSample; Tests.GolangRSM.expressionSample |]

let private generateProgramWithError (size: int) (errorCnt: int) =
    let mutable program = ""
    while program.Length < size do
        program <- program + samples.[rnd.Next(0, samples.Length)]

    let mutable errors = []
    for i in 1..errorCnt do
        let intError = rnd.Next(1, 5)//7)
        errors <- intError.ToString() :: errors
        program <- mkError program (SyntaxError.fromInt intError)

    while program.Length < size do
        program <- program + samples.[rnd.Next(0, samples.Length)]

    let fileName = $"""test_{size.ToString()}_{String.concat "" errors}.go"""
    let filePath = $"""{ErrorRecoveringBenchmark.path}{System.IO.Path.DirectorySeparatorChar}{fileName}"""
    System.IO.File.WriteAllText (filePath, program)


let generateBenchmarkData (sizes: int array) (errorCnts: int array) =
    for size in sizes do
        for errorCnt in errorCnts do
            generateProgramWithError size errorCnt

