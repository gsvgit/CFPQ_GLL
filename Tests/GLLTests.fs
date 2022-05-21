module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Expecto

let runGLL graph startV q =
    let reachable, matchedRanges = GLL.eval graph startV q
    let sppf = matchedRanges.ToSPPF q
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}"

let properties =
  testList "GLL CFPQ Tests with SPPF" [
    testCase "One edge linear graph, one edge linear RSM" <| fun () ->
      let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)|])
      let startV = [|0<graphVertex>|]
      let box = RSMBox(0<rsmState>, HashSet([1<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)|])
      let q = RSM([|box|],box)
      runGLL graph startV q
      
  ]
