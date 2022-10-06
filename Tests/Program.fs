
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto
open CFPQ_GLL.RsmBuilder


let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

  
let go() =
    let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)
                             TerminalEdge(0<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>)                             
                           |])
    let startV = [|0<inputGraphVertex>|]
    let q = Tests.GLLTests.simpleLoopRSMForDyckLanguage ()
    
    let startVertices,_ = graph.ToCfpqCoreGraph (HashSet startV)
    let result = GLL.eval startVertices q GLL.AllPaths

    match result with
    | QueryResult.MatchedRanges ranges -> 
      let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
      let actual = TriplesStoredSPPF (sppf, Dictionary())
      
      actual.ToDot "1.dot"
      
      Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result." 
    0
 
[<EntryPoint>]
let main argv =
    let re1 = RsmBuilder.Sequence (RsmBuilder.Terminal "a", RsmBuilder.Many (RsmBuilder.Terminal "b"))
    let re2 =
        (RsmBuilder.Sequence (RsmBuilder.Terminal "a", RsmBuilder.Many (RsmBuilder.Terminal "b")))
        |> RsmBuilder.Many
        
    let re3 =
        (RsmBuilder.Sequence (RsmBuilder.Terminal "a", RsmBuilder.Terminal "b"))
        |> RsmBuilder.Many
    
    let re4 = many((t "a" ++ t "a") *|* (t "b" ++ t "b"))
    let re5 = opt((t "a" ++ t "a") *|* (t "b" ++ t "b"))
    let re6 = (t "a" *|* t "b" *|* t "c")
              ++ (t "1" *|* t "2" *|* t "3")
              ++ (t "x" *|* t "y" *|* t "z")
              |> many
    
    let startState,finalStates,edges = RsmBuilder.buildRSM re6
    printfn $"StartState: %A{startState}"
    printfn "Final states:"
    for state in finalStates do printf $"%A{state} "
    printfn ""
    printfn "Edges:"
    for edge in edges do
        printfn $"%A{edge}"
    0
    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.DynamicTests.``Simple call``])
   
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()
        
    