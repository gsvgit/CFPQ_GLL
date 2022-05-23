module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto

let runGLLAndCheckResult graph startV q expected =
    let reachable, matchedRanges = GLL.eval graph startV q
    let sppf = matchedRanges.ToSPPF q
    let actual = TriplesStoredSPPF sppf
    Expect.sequenceEqual actual.Nodes (fst expected) "Nodes should be equals."
    Expect.sequenceEqual actual.Edges (snd expected) "Edges should be equals."
    
    

let properties =
  testList "GLL CFPQ Tests with SPPF" [
    testCase "One edge linear graph, one edge linear RSM" <| fun () ->
      let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)|])
      let startV = [|0<graphVertex>|]
      let box = RSMBox(0<rsmState>, HashSet([1<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)|])
      let q = RSM([|box|],box)
      let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          let edges = ResizeArray<_>([|(0,1)|])
          (nodes,edges)
      runGLLAndCheckResult graph startV q expected
      
  ]
