open BenchmarkDotNet.Running
open Benchmarks
open Benchmarks.ErrorRecoveringBenchmark

[<EntryPoint>]
let main _ =
    let summary = BenchmarkRunner.Run<ErrorRecoveringBenchmark.ErrorRecoveringBenchmark>()
    summary.ResultsDirectoryPath |> printfn "Results directory: %s"
    (*benchmarkData |> Array.iter (fun d ->
        printfn $"[{System.DateTime.Now}] Run {d.Name}"
        runGLL d
        printfn $"[{System.DateTime.Now}] Completed {d.Name}"
    )*)
    BenchmarkDataGeneration.generateBenchmarkData [|1000; 5000; 10000; |] [| 0; 1; 2; |]
    
    0
