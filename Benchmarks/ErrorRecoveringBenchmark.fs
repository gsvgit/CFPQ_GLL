module Benchmarks.ErrorRecoveringBenchmark

open System.IO
open CFPQ_GLL
open CFPQ_GLL.Common
open BenchmarkDotNet.Attributes

let path = "/home/gsv/golang"

type BenchmarkData = {
    Name: string
    RSM: unit -> RSM.RSM
    StartVertex: LinearInputGraphVertexBase
    FinishVertex: LinearInputGraphVertexBase
}

let benchmarkData =

    let getDataFromFile (path: string) =
         let graph = File.ReadAllText path |> Tests.LinearGraphReader.mkLinearGraph id
         let startV = 0<inputGraphVertex>
         let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

         let startVertex,mapping = graph.ToCfpqCoreGraph startV
         let finalVertex = mapping[finalV]

         {
            Name = Path.GetFileName path
            RSM = Tests.GolangRSM.golangRSM
            StartVertex = startVertex
            FinishVertex = finalVertex
        }

    Directory.GetFiles(path, "*.go") |> Array.map getDataFromFile


let runGLL (data: BenchmarkData) =
    GLL.errorRecoveringEval data.FinishVertex data.StartVertex (data.RSM ()) GLL.AllPaths |> ignore

[<IterationCount(10)>]
type ErrorRecoveringBenchmark () =

    member val Data = benchmarkData with get, set

    [<ParamsSource(nameof(Data))>]
    [<DefaultValue>] val mutable DataSource: BenchmarkData

    [<Benchmark>]
    member self.bench () = runGLL self.DataSource

