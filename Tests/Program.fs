
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto


let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let test (graph: InputGraph) q =

    let startV = [|0<inputGraphVertex>|]
    graph.ToDot (0, "graph.dot")
    let startVertices,_ = graph.ToCfpqCoreGraph (HashSet startV)
    let result = GLL.defaultEval startVertices q GLL.AllPaths

    match result with
    | QueryResult.MatchedRanges ranges ->
      let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
      let actual = TriplesStoredSPPF (sppf, Dictionary())

      actual.ToDot "sppf.dot"

      Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0

let test2 (graph: InputGraph) q =

    let startV = [|0<inputGraphVertex>|]
    let finalV = [|LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)|]
    graph.ToDot (0, "graph.dot")
    let startVertices,mapping = graph.ToCfpqCoreGraph (HashSet startV)
    let finalVertices = Array.map (fun x -> mapping[x]) finalV |> HashSet
    let result = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths

    match result with
    | QueryResult.MatchedRanges ranges ->
      let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
      let actual = TriplesStoredSPPF (sppf, Dictionary())

      actual.ToDot "sppf.dot"

      Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0




[<EntryPoint>]
let main argv =
    let tests =
        testList "All tests" [
            Tests.LinearGraphReader.``Linear graph creating tests``
            Tests.RSMCalculator.``Calculator RSM tests``
            Tests.EpsilonEdge.``Epsilon edges tests``
            Tests.ErrorRecoveringTest.``Error recovering tests``
        ] |> testSequenced
    Tests.runTestsWithCLIArgs [] [||] tests

    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.GLLTests.``Form V#``])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()

