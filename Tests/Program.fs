open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests

let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let properties =
  testList "Pack-Unpack tests" [
          
    testProperty "unpackGSSVertex - packGSSVertex is identity" <| fun a ->
      if a > 0L<gssVertex> && a <= MAX_VALUE_FOR_GSS_VERTEX
      then
        let unpacked = unpackGSSVertex a
        let packedBack = packGSSVertex unpacked
        a = packedBack
      else true

    testProperty "packGSSVertex - unpackGSSVertex is identity" <| fun inputPosition rsmState ->
      if inputPosition >= 0<graphVertex> && inputPosition <= CFPQ_GLL.InputGraph.GRAPH_VERTEX_MAX_VALUE
         && rsmState >= 0<rsmState> && rsmState <= CFPQ_GLL.RSM.RSM_VERTEX_MAX_VALUE
      then
        let packed = CFPQ_GLL.GSS.packGSSVertex (GSSVertex(inputPosition, rsmState))
        let unpackedBack = CFPQ_GLL.GSS.unpackGSSVertex packed
        inputPosition = unpackedBack.InputPosition
        && rsmState = unpackedBack.RSMState
      else true

  ]
  
let go() =
  let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                           InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,4<graphVertex>)
                           InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                           InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,3<graphVertex>)
                           InputGraph.TerminalEdge(5<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                           InputGraph.TerminalEdge(4<graphVertex>,1<terminalSymbol>,6<graphVertex>)
                           InputGraph.TerminalEdge(10<graphVertex>,2<terminalSymbol>,5<graphVertex>)
                           
                           InputGraph.TerminalEdge(6<graphVertex>,0<terminalSymbol>,7<graphVertex>)
                           InputGraph.TerminalEdge(7<graphVertex>,0<terminalSymbol>,9<graphVertex>)
                           InputGraph.TerminalEdge(8<graphVertex>,0<terminalSymbol>,9<graphVertex>)
                           InputGraph.TerminalEdge(9<graphVertex>,0<terminalSymbol>,10<graphVertex>)
                           InputGraph.TerminalEdge(7<graphVertex>,3<terminalSymbol>,6<graphVertex>)
                           InputGraph.TerminalEdge(10<graphVertex>,4<terminalSymbol>,8<graphVertex>)
                           
                           InputGraph.TerminalEdge(11<graphVertex>,0<terminalSymbol>,12<graphVertex>)
                           InputGraph.TerminalEdge(12<graphVertex>,0<terminalSymbol>,13<graphVertex>)
                           InputGraph.TerminalEdge(14<graphVertex>,0<terminalSymbol>,12<graphVertex>)
                           InputGraph.TerminalEdge(14<graphVertex>,0<terminalSymbol>,15<graphVertex>)
                           InputGraph.TerminalEdge(13<graphVertex>,5<terminalSymbol>,6<graphVertex>)
                           InputGraph.TerminalEdge(10<graphVertex>,6<terminalSymbol>,14<graphVertex>)
                           
                           |])
  
  let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),
              [|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)
                TerminalEdge(0<rsmState>,1<terminalSymbol>,1<rsmState>)
                NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                TerminalEdge(2<rsmState>,2<terminalSymbol>,0<rsmState>)
                
                TerminalEdge(0<rsmState>,3<terminalSymbol>,3<rsmState>)
                NonTerminalEdge(3<rsmState>,0<rsmState>,4<rsmState>)
                TerminalEdge(4<rsmState>,4<terminalSymbol>,0<rsmState>)
                
                TerminalEdge(0<rsmState>,5<terminalSymbol>,5<rsmState>)
                NonTerminalEdge(5<rsmState>,0<rsmState>,6<rsmState>)
                TerminalEdge(6<rsmState>,6<terminalSymbol>,0<rsmState>)
                
                TerminalEdge(0<rsmState>,1<terminalSymbol>,0<rsmState>)
                TerminalEdge(0<rsmState>,3<terminalSymbol>,0<rsmState>)
                TerminalEdge(0<rsmState>,5<terminalSymbol>,0<rsmState>)
                
                |])
  
  let q = RSM([|box|],box)
  let startV = [|0<graphVertex>|]

  let result = GLL.eval graph startV q GLL.AllPaths
  
  match result with
  | QueryResult.MatchedRanges ranges ->
      let sppf = ranges.ToSPPF(startV, q)
      let actual = TriplesStoredSPPF sppf
      
      actual.ToDot "1.dot"
      
      GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result." 
  
[<EntryPoint>]
let main argv =    
    Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [properties; GLLTests.tests])
    //go ()
    //0
    
    