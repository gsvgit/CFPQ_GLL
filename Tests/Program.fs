
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
    let result = GLL.eval startVertices q GLL.AllPaths

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
        ] |> testSequenced
    Tests.runTestsWithCLIArgs [] [||] tests

    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.GLLTests.``Form V#``])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()

