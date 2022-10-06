
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto
open CFPQ_GLL.RsmBuilder


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
    let re1 = t "a" ++ many (t "b")
    let re2 = many (t "a" ++ many (t "b"))                
    let re3 = many (t "a" ++ t "b")           
    let re4 = many((t "a" ++ t "a") *|* (t "b" ++ t "b"))
    let re5 = opt((t "a" ++ t "a") *|* (t "b" ++ t "b"))
    let re6 = (t "a" *|* t "b" *|* t "c")
              ++ (t "1" *|* t "2" *|* t "3")
              ++ (t "x" *|* t "y" *|* t "z")
              |> many
    
    let rsm =
        [
            "S"   =>     (nt "Num" ++ t "+" ++ nt "S")
                     *|* nt "Num"
                     *|* (t "(" ++ nt "S" ++ t ")")
            "Num" =>    ([|1..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                     ++ many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
        ]
        |> build
    rsm.ToDot "rsm.dot"
    0
    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.DynamicTests.``Simple call``])
   
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()

