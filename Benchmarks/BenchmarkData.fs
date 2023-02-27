module Benchmarks.BenchmarkData

open System.IO
open System.Text.RegularExpressions
open CFPQ_GLL
open CFPQ_GLL.Common

let private seed = 100
let private rnd = System.Random(seed)
let workDirectory = "/home/viktor/RiderProjects/CFPQ_GLL/golang"
let configPath = $"{workDirectory}{Path.DirectorySeparatorChar}config"

module ProgramErrors =
    type SyntaxError =
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
                | x -> failwith $"Invalid SyntaxError: {x}"

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

    let generateProgramWithError (size: int) (errorCnt: int) =
        let mutable program = ""
        while program.Length < size do
            program <- program + samples.[rnd.Next(0, samples.Length)]

        let mutable errors = []
        for i in 1..errorCnt do
            let intError = rnd.Next(1, 7)
            errors <- intError.ToString() :: errors
            program <- mkError program (SyntaxError.fromInt intError)

        while program.Length < size do
            program <- program + samples.[rnd.Next(0, samples.Length)]

        program, errors


let private saveProgram (size: int) (errors: string list) (path: string) (program: string)  =
    let fileName = $"""test_{size.ToString()}_{String.concat "" errors}"""
    let filePath = $"""{path}{Path.DirectorySeparatorChar}{fileName}"""
    File.WriteAllText (filePath, program)

let generateBenchmarkData (sizes: int array) (errorCnts: int array) =
    for size in sizes do
        for errorCnt in errorCnts do
            let program, errors = ProgramErrors.generateProgramWithError size errorCnt
            saveProgram size errors workDirectory program

type BenchmarkData = {
    Name: string
    Text: string
    RSM: unit -> RSM.RSM
    StartVertex: LinearInputGraphVertexBase
    FinishVertex: LinearInputGraphVertexBase
    Size: int
    Weight: int
    Errors: string list
    DescriptorsCount: int
    ErrorsCount: int
} with override x.ToString() = x.Name

let mutable (benchmarkData: BenchmarkData array) = [| |]

let private loadBenchmarkDataText () =
    Directory.GetFiles(workDirectory)
    |> Array.filter (fun x ->  x.Contains "test")
    |> Array.map (fun x -> Path.GetFileName x, File.ReadAllText x)

let private getDataFromText (name: string, text: string) =
    let graph = text |> Tests.LinearGraphReader.mkLinearGraph id
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

    let startVertex,mapping = graph.ToCfpqCoreGraph startV
    let finalVertex = mapping[finalV]
    let rsm = Tests.GolangRSM.golangRSM ()
    let result, descriptorsCount = GLL.errorRecoveringEval finalVertex startVertex rsm GLL.AllPaths

    let weight =
        match result with
        | GLL.QueryResult.MatchedRanges _ ->

            let sppf = rsm.OriginalStartState.NonTerminalNodes.ToArray()
            let root = sppf |> Array.filter (fun n -> startVertex = n.LeftPosition && finalVertex = n.RightPosition) |> Array.minBy(fun n -> n.Weight)
            root.Weight |> int
        | _ -> failwith "Result should be MatchedRanges"

    // Expected format: test_{size}_{errors list}
    // Example: test_1000_123 -> (1000, ["KeywordError"; "BracketError"; "OperationError" ])
    let splitData (filename: string) =
        let parts = filename.Split("_")
        assert (Array.length parts = 3)
        let size = int parts[1]
        let errors =
            parts[2]
            |> seq
            |> List.ofSeq
            |> List.map (string << ProgramErrors.SyntaxError.fromInt << int << string)
        size, errors

    let (size, errors) = splitData name

    {
        Name = name
        Text = text
        Size = size
        RSM = Tests.GolangRSM.golangRSM
        StartVertex = startVertex
        FinishVertex = finalVertex
        Weight = weight
        Errors = errors
        ErrorsCount = List.length errors
        DescriptorsCount = descriptorsCount
    }

let private refreshData (data: BenchmarkData) =
     let graph = data.Text |> Tests.LinearGraphReader.mkLinearGraph id
     let startV = 0<inputGraphVertex>
     let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

     let startVertex,mapping = graph.ToCfpqCoreGraph startV
     let finalVertex = mapping[finalV]

     {
        Name = data.Name
        Text = data.Text
        Size = data.Size
        RSM = Tests.GolangRSM.golangRSM
        StartVertex = startVertex
        FinishVertex = finalVertex
        Weight = data.Weight
        Errors = data.Errors
        ErrorsCount = data.ErrorsCount
        DescriptorsCount = data.DescriptorsCount
    }

let private loadBenchmarkData () =
    benchmarkData <- loadBenchmarkDataText () |> Array.map getDataFromText

let reloadBenchmarkData () =
    match benchmarkData with
    | [||] -> benchmarkData <- loadBenchmarkDataText () |> Array.map getDataFromText
    | _ -> ()
    benchmarkData <- benchmarkData |> Array.map refreshData
    benchmarkData
