module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto

let runGLLAndCheckResult graph startV q expected =
    let reachable, matchedRanges = GLL.eval graph startV q
    let sppf = matchedRanges.ToSPPF(startV, q)
    let actual = TriplesStoredSPPF sppf
    Expect.sequenceEqual actual.Nodes (fst expected) "Nodes should be equals."
    Expect.sequenceEqual actual.Edges (snd expected) "Edges should be equals."

let tests =
  testList "GLL CFPQ Tests with SPPF" [
    testCase "Empty graph, Epsilon-only RSM" <| fun () ->
        let graph = InputGraph([||])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[||])
        let q = RSM([|box|],box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          let edges = ResizeArray<_>([|(0,1)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
      
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

    testCase "One edge linear graph, one edge loop RSM" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)|])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "One edge loop graph, one edge loop RSM" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,0<graphVertex>)|])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode(0<graphVertex>,0<rsmState>) )
          nodes.Add(2, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,0<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Two edge linear graph, one edge loop RSM" <| fun () ->
        let graph =
            InputGraph(
                [|
                    InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                    InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                |])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|])
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (4,5); (5,2); (5,6); (6,7)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "One edge linear graph, simple chain call RSM" <| fun () ->
        let graph =
            InputGraph(
                [|
                    InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                |]
                )
        let startV = [|0<graphVertex>|]
        let box1 = RSMBox(0<rsmState>, HashSet([1<rsmState>]),[|NonTerminalEdge(0<rsmState>,2<rsmState>,1<rsmState>)|])
        let box2 = RSMBox(2<rsmState>, HashSet([3<rsmState>]),[|TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)|])
        let q = RSM([|box1; box2|], box1)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,2<rsmState>,1<graphVertex>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,2<rsmState>,3<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2); (2,3)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Two edges linear graph with one pair of brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph =
            InputGraph(
                [|
                    InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                |]
                )
        let startV = [|0<graphVertex>|]
        let box1 = RSMBox(0<rsmState>, HashSet([1<rsmState>]),[|NonTerminalEdge(0<rsmState>,2<rsmState>,1<rsmState>)|])
        let box2 = RSMBox(2<rsmState>, HashSet([3<rsmState>]),[|TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)|])
        let q = RSM([|box1; box2|], box1)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,2<rsmState>,1<graphVertex>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,2<rsmState>,3<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2); (2,3)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
            
    ptestCase "Graph with branch, RSM with nonterminal, nodes reusing" <| fun () ->
        let graph = InputGraph([|
            InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
            InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
            InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,3<graphVertex>)
            InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,4<graphVertex>)
            InputGraph.TerminalEdge(4<graphVertex>,0<terminalSymbol>,2<graphVertex>)
        |])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([2<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                                                               NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)|]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          let edges = ResizeArray<_>([|(0,1)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Graph with branch, RSM is loop DFA, nodes reusing" <| fun () ->
        let graph = InputGraph([|
            InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
            InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
            InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,3<graphVertex>)
            InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,4<graphVertex>)
            InputGraph.TerminalEdge(4<graphVertex>,0<terminalSymbol>,2<graphVertex>)
        |])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))          
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,4<graphVertex>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (4<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,0<terminalSymbol>,3<graphVertex>))
          
          let edges = ResizeArray<_>([|(0,1); (2,3); (4,5); (5,2); (5,6); (6,7); (4,8); (8,9); (9,10); (10,2); (10,11); (11,12); (8,13); (13,14); (15,16); (16,4); (16,17); (17,18)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
  ] |> testSequenced

