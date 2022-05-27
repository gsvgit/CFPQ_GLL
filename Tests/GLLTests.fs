module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto

let dumpResultToConsole (sppf:TriplesStoredSPPF) =
    sppf.Edges |> Seq.iter (fun (x,y) -> printf $"(%i{x},%i{y}); ")
    sppf.Nodes
    |> Seq.iter (fun kvp ->
        match kvp.Value with
        | TriplesStoredSPPFNode.EpsilonNode (_pos,_rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.EpsilonNode (%i{_pos}<graphVertex>,%i{_rsm}<rsmState>))"
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.TerminalNode (%i{_from}<graphVertex>,%i{_terminal}<terminalSymbol>,%i{_to}<graphVertex>))" 
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.NonTerminalNode (%i{_from}<graphVertex>,%i{_nonTerminal}<rsmState>,%i{_to}<graphVertex>))"
        | TriplesStoredSPPFNode.RangeNode (_posFrom, _posTo, _rsmFrom, _rsmTo) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.RangeNode (%i{_posFrom}<graphVertex>,%i{_posTo}<graphVertex>,%i{_rsmFrom}<rsmState>,%i{_rsmTo}<rsmState>))"
        | TriplesStoredSPPFNode.IntermediateNode (_pos, _rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.IntermediateNode (%i{_pos}<graphVertex>,%i{_rsm}<rsmState>))"
        )

let runGLLAndCheckResult graph startV q expected =
    let reachable, matchedRanges = GLL.eval graph startV q
    let sppf = matchedRanges.ToSPPF(startV, q)
    let actual = TriplesStoredSPPF sppf
    Expect.sequenceEqual actual.Nodes (fst expected) "Nodes should be equals."
    Expect.sequenceEqual actual.Edges (snd expected) "Edges should be equals."

let tests =
  let simpleLoopRSMForDyckLanguage =
    let box =
        RSMBox (
            0<rsmState>,
            HashSet([0<rsmState>]),
            [|
              TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
              NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
              TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)
            |])
    RSM([|box|],box)
    
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
      
    testCase "Two loops with common vertex, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,0<graphVertex>)
                                 InputGraph.TerminalEdge(0<graphVertex>,1<terminalSymbol>,0<graphVertex>)                             
                               |])
        let startV = [|0<graphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,0<graphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,0<rsmState>,0<graphVertex>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,1<terminalSymbol>,0<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (8,0); (2,9); (9,10)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
    
    testCase "Minimal worst case, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,0<graphVertex>)                             
                                 InputGraph.TerminalEdge(0<graphVertex>,1<terminalSymbol>,1<graphVertex>)
                                 InputGraph.TerminalEdge(1<graphVertex>,1<terminalSymbol>,0<graphVertex>) |])
        let startV = [|0<graphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,0<graphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,0<rsmState>,1<graphVertex>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,0<rsmState>,0<graphVertex>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,1<terminalSymbol>,1<graphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,0<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,1<terminalSymbol>,0<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (8,9); (9,10); (10,11)
                                       (11,12); (12,5); (12,13); (13,14); (14,0); (10,15); (15,16); (2,17); (17,18)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Second worst case, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                                 InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                                 InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,0<graphVertex>)
                                 
                                 InputGraph.TerminalEdge(0<graphVertex>,1<terminalSymbol>,3<graphVertex>)
                                 InputGraph.TerminalEdge(3<graphVertex>,1<terminalSymbol>,0<graphVertex>) |])
        
        let startV = [|0<graphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,0<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,2<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,1<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (2<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,2<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,0<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,1<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,0<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,0<terminalSymbol>,0<graphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,0<rsmState>,0<graphVertex>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,0<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,2<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,1<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.NonTerminalNode (0<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.IntermediateNode (0<graphVertex>,2<rsmState>))
          nodes.Add(42, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(43, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(44, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,0<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(45, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,0<graphVertex>))
          nodes.Add(46, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,1<terminalSymbol>,3<graphVertex>))
          nodes.Add(48, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(49, TriplesStoredSPPFNode.RangeNode (3<graphVertex>,0<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(50, TriplesStoredSPPFNode.TerminalNode (3<graphVertex>,1<terminalSymbol>,0<graphVertex>))
          nodes.Add(51, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(52, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,1<rsmState>))
          nodes.Add(53, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,0<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (2<graphVertex>,0<rsmState>,0<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (8,9); (9,10); (10,11)
                                       (11,12); (12,13); (13,14); (14,15); (15,16); (16,17); (17,18); (18,19); (19,20)
                                       (18,21); (21,22); (22,23); (23,24); (24,25); (25,26); (26,27); (27,28); (26,29)
                                       (29,30); (30,0); (25,31); (31,32); (32,33); (33,34); (34,35); (35,36); (36,37)
                                       (37,27); (37,38); (38,39); (39,40); (40,41); (41,42); (42,43); (43,5); (43,44)
                                       (44,45); (45,15); (41,46); (46,47); (36,48); (48,32); (48,7); (35,49); (49,50)
                                       (33,5); (31,44); (24,46); (17,51); (51,13); (51,7); (16,49); (14,5); (12,44)
                                       (11,52); (52,19); (52,53); (53,54); (54,34); (10,46); (2,49)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Two concatenated linear pairs of brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                                 InputGraph.TerminalEdge(1<graphVertex>,1<terminalSymbol>,2<graphVertex>)
                                 InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,3<graphVertex>)
                                 InputGraph.TerminalEdge(3<graphVertex>,1<terminalSymbol>,4<graphVertex>) |])
        let startV = [|0<graphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,1<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,1<graphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.EpsilonNode (1<graphVertex>,0<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,1<terminalSymbol>,2<graphVertex>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,2<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,1<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,0<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,0<terminalSymbol>,3<graphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (3<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.NonTerminalNode (3<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (3<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.EpsilonNode (3<graphVertex>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (3<graphVertex>,4<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.TerminalNode (3<graphVertex>,1<terminalSymbol>,4<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (3,12)
                                       (12,13); (14,15); (15,16); (16,17); (17,18); (18,19); (19,2); (19,20); (20,21)
                                       (17,22); (22,23); (23,24); (24,25); (15,26); (26,27)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Linear nested brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                                 InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                                 InputGraph.TerminalEdge(2<graphVertex>,1<terminalSymbol>,3<graphVertex>)
                                 InputGraph.TerminalEdge(3<graphVertex>,1<terminalSymbol>,4<graphVertex>) |])
        let startV = [|0<graphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,2<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.NonTerminalNode (2<graphVertex>,0<rsmState>,2<graphVertex>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.EpsilonNode (2<graphVertex>,0<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,1<terminalSymbol>,3<graphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (3<graphVertex>,4<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.TerminalNode (3<graphVertex>,1<terminalSymbol>,4<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,15); (13,16); (16,17); (17,18); (18,19); (11,20); (20,21)
                                       (3,22); (22,23)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Complex nested linear brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                                 InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                                 InputGraph.TerminalEdge(2<graphVertex>,1<terminalSymbol>,3<graphVertex>)
                                 InputGraph.TerminalEdge(3<graphVertex>,0<terminalSymbol>,4<graphVertex>)
                                 InputGraph.TerminalEdge(4<graphVertex>,1<terminalSymbol>,5<graphVertex>)
                                 InputGraph.TerminalEdge(5<graphVertex>,1<terminalSymbol>,6<graphVertex>)
                               |])
        let startV = [|0<graphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,6<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (5<graphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,5<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,5<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,5<graphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,5<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.IntermediateNode (3<graphVertex>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,2<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,1<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,2<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.NonTerminalNode (2<graphVertex>,0<rsmState>,2<graphVertex>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.EpsilonNode (2<graphVertex>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,1<terminalSymbol>,3<graphVertex>))
          nodes.Add(28, TriplesStoredSPPFNode.RangeNode (3<graphVertex>,4<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(29, TriplesStoredSPPFNode.TerminalNode (3<graphVertex>,0<terminalSymbol>,4<graphVertex>))
          nodes.Add(30, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,4<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(31, TriplesStoredSPPFNode.NonTerminalNode (4<graphVertex>,0<rsmState>,4<graphVertex>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.EpsilonNode (4<graphVertex>,0<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,5<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.TerminalNode (4<graphVertex>,1<terminalSymbol>,5<graphVertex>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (5<graphVertex>,6<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.TerminalNode (5<graphVertex>,1<terminalSymbol>,6<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,15); (15,16); (16,17); (17,18); (18,19); (19,20); (20,21)
                                       (19,22); (22,23); (23,24); (24,25); (17,26); (26,27); (15,28); (28,29); (13,30)
                                       (30,31); (31,32); (32,33); (11,34); (34,35); (3,36); (36,37)|])
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
        let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                                 InputGraph.TerminalEdge(1<graphVertex>,1<terminalSymbol>,2<graphVertex>) |])
        let startV = [|0<graphVertex>|]        
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,1<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,1<graphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.EpsilonNode (1<graphVertex>,0<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,1<terminalSymbol>,2<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (3,12); (12,13)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
            
    testCase "Graph with branch, RSM with nonterminal, nodes reusing" <| fun () ->
        let graph = InputGraph([|
            InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
            InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
            InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,3<graphVertex>)
            InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,4<graphVertex>)
            InputGraph.TerminalEdge(4<graphVertex>,0<terminalSymbol>,2<graphVertex>)
        |])
        let startV = [|0<graphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([2<rsmState>]),
                         [|
                           TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                           NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                           TerminalEdge(0<rsmState>,0<terminalSymbol>,2<rsmState>)
                         |]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,2<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,2<graphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,1<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,4<graphVertex>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,2<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.NonTerminalNode (4<graphVertex>,0<rsmState>,2<graphVertex>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,2<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (4<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,1<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (4<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,1<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,2<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (4<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (2<graphVertex>,0<rsmState>,3<graphVertex>))
          nodes.Add(31, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,0<terminalSymbol>,3<graphVertex>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,1<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,2<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,4<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,1<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.NonTerminalNode (1<graphVertex>,0<rsmState>,4<graphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,4<graphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (3,6); (6,7); (7,8); (8,9); (8,10); (10,11); (11,12)
                                       (10,13); (13,14); (14,15); (15,16); (17,18); (18,4); (18,19); (19,20); (20,21)
                                       (21,22); (22,11); (22,23); (23,24); (24,25); (25,26); (26,27); (27,28); (26,29)
                                       (29,30); (30,31); (31,32); (21,33); (33,34); (34,35); (33,29); (36,37); (37,4)
                                       (37,38); (38,39); (39,40); (40,41)
                                     |])
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
        
    testCase "Minimal example of interprocedural control flow." <| fun () ->
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
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,0<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<graphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (5<graphVertex>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,5<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (10<graphVertex>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,10<graphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (6<graphVertex>,1<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,6<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (1<graphVertex>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (1<graphVertex>,4<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (1<graphVertex>,0<terminalSymbol>,4<graphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,6<graphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (4<graphVertex>,1<terminalSymbol>,6<graphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,10<graphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (6<graphVertex>,0<rsmState>,10<graphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,10<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.IntermediateNode (9<graphVertex>,0<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,9<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (7<graphVertex>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,7<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (6<graphVertex>,0<terminalSymbol>,7<graphVertex>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (7<graphVertex>,9<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (7<graphVertex>,0<terminalSymbol>,9<graphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.IntermediateNode (8<graphVertex>,0<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,8<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (10<graphVertex>,4<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,10<graphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (6<graphVertex>,3<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,6<graphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (7<graphVertex>,0<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (7<graphVertex>,6<graphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.TerminalNode (7<graphVertex>,3<terminalSymbol>,6<graphVertex>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (6<graphVertex>,10<graphVertex>,3<rsmState>,4<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.NonTerminalNode (6<graphVertex>,0<rsmState>,10<graphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (10<graphVertex>,8<graphVertex>,4<rsmState>,0<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.TerminalNode (10<graphVertex>,4<terminalSymbol>,8<graphVertex>))
          nodes.Add(42, TriplesStoredSPPFNode.RangeNode (8<graphVertex>,9<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(43, TriplesStoredSPPFNode.TerminalNode (8<graphVertex>,0<terminalSymbol>,9<graphVertex>))
          nodes.Add(44, TriplesStoredSPPFNode.RangeNode (9<graphVertex>,10<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(45, TriplesStoredSPPFNode.TerminalNode (9<graphVertex>,0<terminalSymbol>,10<graphVertex>))
          nodes.Add(46, TriplesStoredSPPFNode.RangeNode (10<graphVertex>,5<graphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (10<graphVertex>,2<terminalSymbol>,5<graphVertex>))
          nodes.Add(48, TriplesStoredSPPFNode.RangeNode (5<graphVertex>,2<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(49, TriplesStoredSPPFNode.TerminalNode (5<graphVertex>,0<terminalSymbol>,2<graphVertex>))
          nodes.Add(50, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,1<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(51, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,0<terminalSymbol>,1<graphVertex>))
          nodes.Add(52, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(53, TriplesStoredSPPFNode.IntermediateNode (2<graphVertex>,0<rsmState>))
          nodes.Add(54, TriplesStoredSPPFNode.RangeNode (2<graphVertex>,3<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(55, TriplesStoredSPPFNode.TerminalNode (2<graphVertex>,0<terminalSymbol>,3<graphVertex>))
          nodes.Add(56, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,6<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(57, TriplesStoredSPPFNode.IntermediateNode (4<graphVertex>,0<rsmState>))
          nodes.Add(58, TriplesStoredSPPFNode.RangeNode (4<graphVertex>,6<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(59, TriplesStoredSPPFNode.TerminalNode (4<graphVertex>,1<terminalSymbol>,6<graphVertex>))
          nodes.Add(60, TriplesStoredSPPFNode.IntermediateNode (7<graphVertex>,0<rsmState>))
          nodes.Add(61, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,7<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(62, TriplesStoredSPPFNode.IntermediateNode (6<graphVertex>,0<rsmState>))
          nodes.Add(63, TriplesStoredSPPFNode.RangeNode (7<graphVertex>,6<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(64, TriplesStoredSPPFNode.TerminalNode (7<graphVertex>,3<terminalSymbol>,6<graphVertex>))
          nodes.Add(65, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,8<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(66, TriplesStoredSPPFNode.IntermediateNode (10<graphVertex>,4<rsmState>))
          nodes.Add(67, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,10<graphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(68, TriplesStoredSPPFNode.IntermediateNode (6<graphVertex>,3<rsmState>))
          nodes.Add(69, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,6<graphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(70, TriplesStoredSPPFNode.IntermediateNode (7<graphVertex>,0<rsmState>))
          nodes.Add(71, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,9<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(72, TriplesStoredSPPFNode.IntermediateNode (7<graphVertex>,0<rsmState>))
          nodes.Add(73, TriplesStoredSPPFNode.IntermediateNode (8<graphVertex>,0<rsmState>))
          nodes.Add(74, TriplesStoredSPPFNode.RangeNode (0<graphVertex>,10<graphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(75, TriplesStoredSPPFNode.IntermediateNode (9<graphVertex>,0<rsmState>))
          
          let edges = ResizeArray<_>([|(0,1); (2,3); (2,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,2); (14,15); (15,16); (12,17); (17,18); (10,19); (19,20)
                                       (20,21); (21,22); (22,23); (23,24); (24,25); (25,26); (24,27); (27,28); (23,29)
                                       (29,30); (30,31); (31,32); (32,33); (33,34); (34,35); (35,25); (35,36); (36,37)
                                       (33,38); (38,39); (39,21); (31,40); (40,41); (29,42); (42,43); (22,44); (44,45)
                                       (8,46); (46,47); (6,48); (48,49); (4,50); (50,51); (52,53); (53,5); (53,54)
                                       (54,55); (56,57); (57,13); (57,58); (58,59); (56,60); (60,61); (61,62); (62,56)
                                       (62,25); (60,63); (63,64); (65,66); (66,67); (67,68); (68,69); (69,70); (70,61)
                                       (70,36); (68,38); (66,40); (71,72); (72,61); (72,27); (71,73); (73,65); (73,42)
                                       (74,75); (75,71); (75,44)
                                     |])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
  ] |> testSequenced

