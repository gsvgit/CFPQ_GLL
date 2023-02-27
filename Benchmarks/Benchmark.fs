module Benchmarks.Benchmark

open System.IO
open CFPQ_GLL
open CFPQ_GLL.Common
open BenchmarkDotNet.Attributes


let runGLL (data: BenchmarkData.BenchmarkData) =
    GLL.errorRecoveringEval data.FinishVertex data.StartVertex (data.RSM ()) GLL.AllPaths |> ignore


[<Config(typeof<BenchmarkConfig.BenchmarkConfig>)>]
type ErrorRecoveringBenchmark () =

    member val Data = BenchmarkData.reloadBenchmarkData () with get, set

    [<ParamsSource(nameof(Data))>]
    [<DefaultValue>] val mutable DataSource: BenchmarkData.BenchmarkData

    [<IterationSetup>]
    member this.ReloadGraphs () = this.Data <- BenchmarkData.reloadBenchmarkData ()

    [<Benchmark>]
    member this.ErrorRecoveringGll () = runGLL this.DataSource

