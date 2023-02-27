open BenchmarkDotNet.Running
open Benchmarks
open Benchmarks.Benchmark

let parseConfig path =
    let confText = System.IO.File.ReadAllText path
    let lines = confText.Split "\n"
    let sizes = lines[0].Split " " |> Seq.map int |> Array.ofSeq
    let errors = lines[1].Split " " |> seq |> Seq.map int |> Array.ofSeq
    sizes, errors

[<EntryPoint>]
let main args =
    // Все еще нужно хардкодить путь в DataGeneration (из-за ограничений BenchmarkDotNet)
    // Конфиг для генерации данных читается из той же папки
    match args[0] with
    | "--run" ->
        let summary = BenchmarkRunner.Run<ErrorRecoveringBenchmark>()
        printfn $"Result directory: {summary.ResultsDirectoryPath}"
    | "--generate" ->
        for file in System.IO.Directory.GetFiles(BenchmarkData.workDirectory)
            do if file.Contains "test" then System.IO.File.Delete file
        let sizes, errors = parseConfig BenchmarkData.configPath
        BenchmarkData.generateBenchmarkData sizes errors
    | "--debug" ->
        printfn "debug"
    | _ -> failwith "Unexpected argument"
    0
