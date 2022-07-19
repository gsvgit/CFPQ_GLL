module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests.InputGraph

(*
let dumpResultToConsole (sppf:TriplesStoredSPPF) =
    sppf.Edges |> Seq.iter (fun (x,y) -> printf $"(%i{x},%i{y}); ")
    sppf.Nodes
    |> Seq.iter (fun kvp ->
        match kvp.Value with
        | TriplesStoredSPPFNode.EpsilonNode (_pos,_rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.EpsilonNode (%i{_pos}<inputGraphVertex>,%i{_rsm}<rsmState>))"
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.TerminalNode (%i{_from}<inputGraphVertex>,%i{_terminal}<terminalSymbol>,%i{_to}<inputGraphVertex>))" 
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.NonTerminalNode (%i{_from}<inputGraphVertex>,%i{_nonTerminal}<rsmState>,%i{_to}<inputGraphVertex>))"
        | TriplesStoredSPPFNode.RangeNode (_posFrom, _posTo, _rsmFrom, _rsmTo) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.RangeNode (%i{_posFrom}<inputGraphVertex>,%i{_posTo}<inputGraphVertex>,%i{_rsmFrom}<rsmState>,%i{_rsmTo}<rsmState>))"
        | TriplesStoredSPPFNode.IntermediateNode (_pos, _rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.IntermediateNode (%i{_pos}<inputGraphVertex>,%i{_rsm}<rsmState>))"
        )
*)

let runGLLAndCheckResult graph (startV:array<_>) q expected =
    let result = eval graph (HashSet startV) q AllPaths
    match result with
    | QueryResult.MatchedRanges ranges ->
        let sppf = ranges.ToSPPF(q, startV)
        let actual = TriplesStoredSPPF sppf
        Expect.sequenceEqual actual.Nodes (fst expected) "Nodes should be equals."
        Expect.sequenceEqual actual.Edges (snd expected) "Edges should be equals."
    | _ -> failwith "Result should be MatchedRanges"

let simpleLoopRSMForDyckLanguage =
    let box =
        RSMBox (
            0<rsmState>,
            HashSet([0<rsmState>]),
            [|
              RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
              RSM.NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
              RSM.TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)
            |])
    RSM([|box|],box)
    
let tests =  
    
  testList "GLL CFPQ Tests with SPPF" [
    testCase "Empty graph, Epsilon-only RSM" <| fun () ->
        let graph = InputGraph([||])
        let startV = [|0<inputGraphVertex>|]
        graph.AddVertex startV.[0]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[||])
        let q = RSM([|box|],box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          let edges = ResizeArray<_>([|(0,1)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
      
    testCase "Two loops with common vertex, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)
                                 TerminalEdge(0<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>)                             
                               |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>, 0<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>, 0<inputGraphVertex>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>, 0<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (8,0); (2,9); (9,10)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
    
    testCase "Minimal worst case, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)                             
                                 TerminalEdge(0<inputGraphVertex>, 1<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>, 0<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 1<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>, 1<inputGraphVertex>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 0<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>, 0<inputGraphVertex>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>, 1<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>, 1<inputGraphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>, 0<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (8,9); (9,10); (10,11)
                                       (11,12); (12,5); (12,13); (13,14); (14,0); (10,15); (15,16); (2,17); (17,18)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Second worst case, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>,0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>,0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>,0<terminalSymbol>, 0<inputGraphVertex>)
                                 
                                 TerminalEdge(0<inputGraphVertex>,1<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>,1<terminalSymbol>, 0<inputGraphVertex>) |])
        
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(42, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(43, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(44, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,0<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(45, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(46, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(48, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(49, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(50, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,0<inputGraphVertex>))
          nodes.Add(51, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(52, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(53, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
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
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>, 1<terminalSymbol>, 4<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.EpsilonNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.EpsilonNode (3<inputGraphVertex>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,4<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (3,12)
                                       (12,13); (14,15); (15,16); (16,17); (17,18); (18,19); (19,2); (19,20); (20,21)
                                       (17,22); (22,23); (23,24); (24,25); (15,26); (26,27)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Linear nested brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 1<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>, 1<terminalSymbol>, 4<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.EpsilonNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,4<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,15); (13,16); (16,17); (17,18); (18,19); (11,20); (20,21)
                                       (3,22); (22,23)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Complex nested linear brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 1<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
                                 TerminalEdge(4<inputGraphVertex>, 1<terminalSymbol>, 5<inputGraphVertex>)
                                 TerminalEdge(5<inputGraphVertex>, 1<terminalSymbol>, 6<inputGraphVertex>)
                               |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,5<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,5<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.EpsilonNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(28, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(29, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(30, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,4<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(31, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.EpsilonNode (4<inputGraphVertex>,0<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,5<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,6<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,15); (15,16); (16,17); (17,18); (18,19); (19,20); (20,21)
                                       (19,22); (22,23); (23,24); (24,25); (17,26); (26,27); (15,28); (28,29); (13,30)
                                       (30,31); (31,32); (32,33); (11,34); (34,35); (3,36); (36,37)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "One edge linear graph, one edge linear RSM" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)|])
        let startV = [|0<inputGraphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([1<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)|])
        let q = RSM([|box|],box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected

    testCase "One edge linear graph, one edge loop RSM" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)|])
        let startV = [|0<inputGraphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "One edge loop graph, one edge loop RSM" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)|])
        let startV = [|0<inputGraphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode(0<inputGraphVertex>,0<rsmState>) )
          nodes.Add(2, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (0,2)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Two edge linear graph, one edge loop RSM" <| fun () ->
        let graph =
            InputGraph(
                [|
                    TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                    TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                |])
        let startV = [|0<inputGraphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|])
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (4,5); (5,2); (5,6); (6,7)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "One edge linear graph, simple chain call RSM" <| fun () ->
        let graph =
            InputGraph(
                [|
                    TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                |]
                )
        let startV = [|0<inputGraphVertex>|]
        let box1 = RSMBox(0<rsmState>, HashSet([1<rsmState>]),[|NonTerminalEdge(0<rsmState>,2<rsmState>,1<rsmState>)|])
        let box2 = RSMBox(2<rsmState>, HashSet([3<rsmState>]),[|RSM.TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)|])
        let q = RSM([|box1; box2|], box1)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,2<rsmState>,1<inputGraphVertex>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,3<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2); (2,3)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Two edges linear graph with one pair of brackets, simple loop RSM for Dyck language" <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 2<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]        
        let q = simpleLoopRSMForDyckLanguage
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.EpsilonNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11); (3,12); (12,13)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
            
    testCase "Graph with branch, RSM with nonterminal, nodes reusing" <| fun () ->
        let graph = InputGraph([|
            TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
            TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
            TerminalEdge(4<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
        |])
        let startV = [|0<inputGraphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([2<rsmState>]),
                         [|
                           RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                           RSM.NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                           RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,2<rsmState>)
                         |]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,1<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,1<rsmState>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(31, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(35, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(37, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(39, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (2,3); (3,4); (4,5); (3,6); (6,7); (7,8); (8,9); (10,11); (11,4); (11,12)
                                       (12,13); (13,14); (14,15); (14,16); (16,17); (17,18); (16,19); (19,20); (20,21)
                                       (21,22); (23,24); (24,4); (24,25); (25,26); (26,27); (27,28); (28,17); (28,29)
                                       (29,30); (30,31); (31,32); (32,33); (33,34); (32,35); (35,36); (36,37); (37,38)
                                       (27,39); (39,40); (40,41); (39,35)
                                     |])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Graph with branch, RSM is loop DFA, nodes reusing" <| fun () ->
        let graph = InputGraph([|
            TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
            TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
            TerminalEdge(4<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
        |])
        let startV = [|0<inputGraphVertex>|]
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))          
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          
          let edges = ResizeArray<_>([|(0,1); (2,3); (4,5); (5,2); (5,6); (6,7); (4,8); (8,9); (9,10); (10,2); (10,11); (11,12); (8,13); (13,14); (15,16); (16,4); (16,17); (17,18)|])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
        
    testCase "Minimal example of interprocedural control flow." <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(5<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(4<inputGraphVertex>, 1<terminalSymbol>, 6<inputGraphVertex>)
                                 TerminalEdge(10<inputGraphVertex>,2<terminalSymbol>, 5<inputGraphVertex>)
                                 
                                 TerminalEdge(6<inputGraphVertex>, 0<terminalSymbol>,7<inputGraphVertex>)
                                 TerminalEdge(7<inputGraphVertex>, 0<terminalSymbol>,9<inputGraphVertex>)
                                 TerminalEdge(8<inputGraphVertex>, 0<terminalSymbol>,9<inputGraphVertex>)
                                 TerminalEdge(9<inputGraphVertex>, 0<terminalSymbol>,10<inputGraphVertex>)
                                 TerminalEdge(7<inputGraphVertex>, 3<terminalSymbol>,6<inputGraphVertex>)
                                 TerminalEdge(10<inputGraphVertex>,4<terminalSymbol>,8<inputGraphVertex>)
                                 
                                 TerminalEdge(11<inputGraphVertex>, 0<terminalSymbol>, 12<inputGraphVertex>)
                                 TerminalEdge(12<inputGraphVertex>, 0<terminalSymbol>, 13<inputGraphVertex>)
                                 TerminalEdge(14<inputGraphVertex>, 0<terminalSymbol>, 12<inputGraphVertex>)
                                 TerminalEdge(14<inputGraphVertex>, 0<terminalSymbol>, 15<inputGraphVertex>)
                                 TerminalEdge(13<inputGraphVertex>, 5<terminalSymbol>, 6<inputGraphVertex>)
                                 TerminalEdge(10<inputGraphVertex>, 6<terminalSymbol>, 14<inputGraphVertex>)
                                 
                                 |])
        
        let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),
                    [|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)
                      RSM.TerminalEdge(0<rsmState>,1<terminalSymbol>,1<rsmState>)
                      RSM.NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                      RSM.TerminalEdge(2<rsmState>,2<terminalSymbol>,0<rsmState>)
                      
                      RSM.TerminalEdge(0<rsmState>,3<terminalSymbol>,3<rsmState>)
                      RSM.NonTerminalEdge(3<rsmState>,0<rsmState>,4<rsmState>)
                      RSM.TerminalEdge(4<rsmState>,4<terminalSymbol>,0<rsmState>)
                      
                      RSM.TerminalEdge(0<rsmState>,5<terminalSymbol>,5<rsmState>)
                      RSM.NonTerminalEdge(5<rsmState>,0<rsmState>,6<rsmState>)
                      RSM.TerminalEdge(6<rsmState>,6<terminalSymbol>,0<rsmState>)
                      
                      RSM.TerminalEdge(0<rsmState>,1<terminalSymbol>,0<rsmState>)
                      RSM.TerminalEdge(0<rsmState>,3<terminalSymbol>,0<rsmState>)
                      RSM.TerminalEdge(0<rsmState>,5<terminalSymbol>,0<rsmState>)
                      
                      |])
        
        let q = RSM([|box|],box)
        let startV = [|0<inputGraphVertex>|]
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(1, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (10<inputGraphVertex>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,1<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,10<inputGraphVertex>,1<rsmState>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,0<rsmState>,10<inputGraphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,0<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,9<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,0<terminalSymbol>,7<inputGraphVertex>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,9<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,0<terminalSymbol>,9<inputGraphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,8<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (10<inputGraphVertex>,4<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,3<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,3<terminalSymbol>,6<inputGraphVertex>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,10<inputGraphVertex>,3<rsmState>,4<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,0<rsmState>,10<inputGraphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (10<inputGraphVertex>,8<inputGraphVertex>,4<rsmState>,0<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.TerminalNode (10<inputGraphVertex>,4<terminalSymbol>,8<inputGraphVertex>))
          nodes.Add(42, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,9<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(43, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,0<terminalSymbol>,9<inputGraphVertex>))
          nodes.Add(44, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(45, TriplesStoredSPPFNode.TerminalNode (9<inputGraphVertex>,0<terminalSymbol>,10<inputGraphVertex>))
          nodes.Add(46, TriplesStoredSPPFNode.RangeNode (10<inputGraphVertex>,5<inputGraphVertex>,2<rsmState>,0<rsmState>))
          nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (10<inputGraphVertex>,2<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(48, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(49, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(50, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(51, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(52, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(53, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,0<rsmState>))
          nodes.Add(54, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(55, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
          nodes.Add(56, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(57, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(58, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,0<rsmState>))
          nodes.Add(59, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(60, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,3<terminalSymbol>,6<inputGraphVertex>))
          nodes.Add(61, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,9<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(62, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(63, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(64, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,8<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(65, TriplesStoredSPPFNode.IntermediateNode (10<inputGraphVertex>,4<rsmState>))
          nodes.Add(66, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(67, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,3<rsmState>))
          nodes.Add(68, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(69, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(70, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(71, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,0<rsmState>))
          nodes.Add(72, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(73, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(74, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(75, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          
          let edges = ResizeArray<_>([|(0,1); (2,3); (2,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,2); (14,15); (15,16); (12,17); (17,18); (10,19); (19,20)
                                       (20,21); (21,22); (22,23); (23,24); (24,25); (25,26); (24,27); (27,28); (23,29)
                                       (29,30); (30,31); (31,32); (32,33); (33,34); (34,35); (35,25); (35,36); (36,37)
                                       (33,38); (38,39); (39,21); (31,40); (40,41); (29,42); (42,43); (22,44); (44,45)
                                       (8,46); (46,47); (6,48); (48,49); (4,50); (50,51); (52,53); (53,13); (53,54)
                                       (54,55); (52,56); (56,57); (57,58); (58,52); (58,25); (56,59); (59,60); (61,62)
                                       (62,57); (62,27); (61,63); (63,64); (64,65); (65,66); (66,67); (67,68); (68,69)
                                       (69,57); (69,36); (67,38); (65,40); (63,42); (70,71); (71,61); (71,44); (72,73)
                                       (73,5); (73,74); (74,75)
                                     |])
          (nodes,edges)
        runGLLAndCheckResult graph startV q expected
  ] |> testSequenced

