
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
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
      let sppf = ranges.ToSPPF startV
      let actual = TriplesStoredSPPF sppf
      
      actual.ToDot "1.dot"
      
      //GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result." 
    0
 
[<EntryPoint>]
let main argv =    
  let allTests = testList "all tests" [Tests.GLLTests.tests]
  
  if argv |> Array.contains "--list-tests" then
      printfn "Available tests:"
      allTests
      |> Test.toTestCodeList
      |> List.iter (fun test ->
          let fullPath = String.concat "/" test.name
          printfn "%s" fullPath)
      0
  else
    Tests.runTestsWithCLIArgs [] [||] allTests
    //go ()
        
    