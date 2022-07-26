﻿
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto


let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

  
let go() =
    let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)
                             TerminalEdge(0<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>)                             
                           |])
    let startV = [|0<inputGraphVertex>|]
    let q = Tests.GLLTests.simpleLoopRSMForDyckLanguage

    let result = GLL.eval graph (HashSet startV) q GLL.AllPaths

    match result with
    | QueryResult.MatchedRanges ranges -> 
      let sppf = ranges.ToSPPF(q, startV)
      let actual = TriplesStoredSPPF sppf
      
      actual.ToDot "1.dot"
      
      //GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result." 
    0
 
[<EntryPoint>]
let main argv =
    let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)|])
    let startV = [|0<inputGraphVertex>|]
    let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
    let q = RSM([|box|], box)
    let r = eval graph (HashSet startV) q AllPaths
    0
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()
        
    