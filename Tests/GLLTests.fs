module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests.InputGraph

let makeRsmBox (statesMapping: Dictionary<int<rsmState>, IRsmState>, startSate:int<rsmState>, finalStates: HashSet<int<rsmState>>, edges: array<RSMEdges>) =
    let box = RSMBox()
    let getState stateId =
        let exists, state = statesMapping.TryGetValue stateId
        if exists
        then state
        else
            let state = RsmVertex((stateId = startSate), finalStates.Contains stateId)
            statesMapping.Add(stateId, state)
            box.AddState state
            state
    getState startSate |> ignore
    for edge in edges do
        let startState = getState edge.StartState
        let finalState = getState edge.FinalState
        match edge with
        | NonTerminalEdge(_from,_nonTerm,_to ) ->
            let nonTerm = getState _nonTerm
            let exists,finalStates = startState.OutgoingNonTerminalEdges.TryGetValue nonTerm
            if exists
            then
                finalStates.Add finalState |> ignore
            else 
                startState.OutgoingNonTerminalEdges.Add(nonTerm, HashSet [|finalState|])
        | RSMEdges.TerminalEdge (_from,_term,_to ) ->            
            let exists,finalStates = startState.OutgoingTerminalEdges.TryGetValue _term
            if exists
            then
                finalStates.Add finalState |> ignore
            else 
                startState.OutgoingTerminalEdges.Add(_term, HashSet [|finalState|])
                
    box, statesMapping
        
let dumpResultToConsole (sppf:TriplesStoredSPPF<_>) =
    sppf.Edges |> Seq.iter (fun (x,y) -> printf $"(%i{x},%i{y}); ")
    printfn ""
    sppf.Nodes
    |> Seq.iter (fun kvp ->
        match kvp.Value with
        | TriplesStoredSPPFNode.EpsilonNode (_pos,_rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.EpsilonNode (%i{_pos}<inputGraphVertex>,%i{_rsm}<rsmState>))"
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.TerminalNode (%i{_from}<inputGraphVertex>,%i{_terminal}<terminalSymbol>,%i{_to}<inputGraphVertex>))" 
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.NonTerminalNode (%i{_from}<inputGraphVertex>,%i{_nonTerminal}<rsmState>,%i{_to}<inputGraphVertex>))"
        | TriplesStoredSPPFNode.RangeNode (_posFrom, _posTo, _rsmFrom, _rsmTo) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.RangeNode (%i{_posFrom}<inputGraphVertex>,%i{_posTo}<inputGraphVertex>,%i{_rsmFrom}<rsmState>,%i{_rsmTo}<rsmState>))"
        | TriplesStoredSPPFNode.IntermediateNode (_pos, _rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.IntermediateNode (%i{_pos}<inputGraphVertex>,%i{_rsm}<rsmState>))"
        )


let runGLLAndCheckResult (testName:string) (graph:InputGraph) (startV:array<_>) (q:RSM) expected =
    let validDotFileName =
        testName.Replace(',', ' ').Replace(' ', '_') + ".dot"
    let startVertices = graph.ToCfpqCoreGraph (HashSet startV)
    let result = eval startVertices q AllPaths
    match result with
    | QueryResult.MatchedRanges ranges ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let actual = TriplesStoredSPPF(sppf, Dictionary())
        //dumpResultToConsole actual
        //actual.ToDot validDotFileName
        Expect.sequenceEqual actual.Nodes (fst expected) "Nodes should be equals."
        Expect.sequenceEqual actual.Edges (snd expected) "Edges should be equals."
    | _ -> failwith "Result should be MatchedRanges"

let simpleLoopRSMForDyckLanguage () =
    let box,_ =
        makeRsmBox (
            Dictionary(),
            0<rsmState>,
            HashSet([0<rsmState>]),
            [|
              RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
              RSM.NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
              RSM.TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)
            |])
    RSM([|box|],box)

let ``One edge loop graph, one edge loop RSM`` =
    let testName = "One edge loop graph, one edge loop RSM"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)|])
        let startV = [|0<inputGraphVertex>|]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2); (1,3);|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected

let ``Empty graph, Epsilon-only RSM`` =
    let testName = "Empty graph, Epsilon-only RSM"
    testCase testName <| fun () ->
        let graph = InputGraph([||])
        let startV = [|0<inputGraphVertex>|]
        graph.AddVertex startV.[0]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([0<rsmState>]),[||])
        let q = RSM([|box|],box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))

          let edges = ResizeArray<_>([|(0,1); (1,2);|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
    
let ``One edge linear graph, one edge linear RSM`` =
    let testName = "One edge linear graph, one edge linear RSM"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)|])
        let startV = [|0<inputGraphVertex>|]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([1<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)|])
        let q = RSM([|box|],box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected

let ``One edge linear graph, one edge loop RSM`` =
    let testName = "One edge linear graph, one edge loop RSM"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)|])
        let startV = [|0<inputGraphVertex>|]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Two edge linear graph, one edge loop RSM`` =
    let testName = "Two edge linear graph, one edge loop RSM"
    testCase testName <| fun () ->
        let graph =
            InputGraph(
                [|
                    TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                    TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                |])
        let startV = [|0<inputGraphVertex>|]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|])
        let q = RSM([|box|], box)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected

let ``Minimal example of interprocedural control flow`` =
    let testName = "Minimal example of interprocedural control flow" 
    testCase testName <| fun () ->
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
        
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([0<rsmState>]),
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
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,1<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,2<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,6<inputGraphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,8<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,0<terminalSymbol>,8<inputGraphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,0<terminalSymbol>,7<inputGraphVertex>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,0<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,9<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,3<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,4<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,3<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,4<rsmState>,3<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,9<inputGraphVertex>,3<rsmState>,0<rsmState>))
          nodes.Add(42, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,4<terminalSymbol>,9<inputGraphVertex>))
          nodes.Add(43, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(44, TriplesStoredSPPFNode.TerminalNode (9<inputGraphVertex>,0<terminalSymbol>,7<inputGraphVertex>))
          nodes.Add(45, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(46, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(47, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(48, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,2<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(49, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(50, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(51, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(52, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(53, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,6<inputGraphVertex>))
          nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,5<inputGraphVertex>))
          nodes.Add(55, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(56, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,0<rsmState>))
          nodes.Add(57, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(58, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(59, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(60, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,8<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(61, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,0<rsmState>))
          nodes.Add(62, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(63, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,3<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(64, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,8<inputGraphVertex>))
          nodes.Add(65, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,5<inputGraphVertex>))
          nodes.Add(66, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(67, TriplesStoredSPPFNode.EpsilonNode (5<inputGraphVertex>,0<rsmState>))
          nodes.Add(68, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(69, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,8<inputGraphVertex>))
          nodes.Add(70, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,7<inputGraphVertex>))
          nodes.Add(71, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,9<inputGraphVertex>))
          nodes.Add(72, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,9<inputGraphVertex>))
          nodes.Add(73, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,9<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(74, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,3<rsmState>))
          nodes.Add(75, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,3<rsmState>))
          nodes.Add(76, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,4<rsmState>))
          nodes.Add(77, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,4<rsmState>))
          nodes.Add(78, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(79, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,7<inputGraphVertex>))
          nodes.Add(80, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(81, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,0<rsmState>))
          nodes.Add(82, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,0<rsmState>))
          nodes.Add(83, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(84, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(85, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,0<rsmState>))
          nodes.Add(86, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(87, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(88, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,10<inputGraphVertex>))
          nodes.Add(89, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(90, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(91, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,10<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(92, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,10<inputGraphVertex>))
          
          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (4,6); (6,7); (7,8); (8,9); (9,10); (10,11); (11,12)
                                       (12,13); (13,14); (14,15); (15,16); (16,4); (16,17); (17,18); (14,19); (19,20)
                                       (12,21); (21,22); (22,23); (23,24); (24,25); (25,26); (26,27); (27,28); (26,29)
                                       (29,30); (25,31); (31,32); (32,33); (33,34); (34,35); (35,36); (36,37); (37,27)
                                       (37,38); (38,39); (35,40); (40,22); (33,41); (41,42); (31,43); (43,44); (24,45)
                                       (45,46); (10,47); (47,48); (8,49); (49,50); (6,51); (51,52); (53,15); (54,55)
                                       (55,56); (56,15); (56,57); (57,58); (55,59); (59,60); (60,61); (61,55); (61,27)
                                       (59,62); (62,63); (64,60); (65,66); (66,67); (66,68); (68,27); (68,62); (69,27)
                                       (70,25); (71,32); (72,73); (73,74); (74,75); (75,76); (76,77); (77,78); (78,60)
                                       (78,38); (76,40); (74,41); (79,80); (80,81); (81,60); (81,29); (80,82); (82,73)
                                       (82,43); (83,84); (84,85); (85,80); (85,45); (86,9); (87,7); (88,89); (89,90)
                                       (90,7); (90,91); (91,92)
                                     |])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Two concatenated linear pairs of brackets, simple loop RSM for Dyck language`` =
    let testName = "Two concatenated linear pairs of brackets, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>, 1<terminalSymbol>, 4<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.EpsilonNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.EpsilonNode (3<inputGraphVertex>,0<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(28, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,4<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,9); (9,10); (10,11); (11,12); (10,13)
                                       (13,3); (8,14); (14,15); (16,17); (17,18); (19,20); (20,21); (21,22); (22,23)
                                       (23,24); (24,25); (25,7); (25,26); (26,27); (23,28); (28,16); (21,29); (29,30)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Two loops with common vertex, simple loop RSM for Dyck language`` =
    let testName = "Two loops with common vertex, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)
                                 TerminalEdge(0<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>)                             
                               |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,0<inputGraphVertex>))


          let edges = ResizeArray<_>([|(0,1); (1,2); (1,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,0); (3,9); (9,10)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Minimal worst case, simple loop RSM for Dyck language`` =
    let testName = "Minimal worst case, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 0<inputGraphVertex>)                             
                                 TerminalEdge(0<inputGraphVertex>, 1<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 0<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,0<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,0<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (1,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11)
                                       (11,12); (12,13); (13,6); (13,14); (14,0); (11,15); (15,16); (3,17); (17,18)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Second worst case, simple loop RSM for Dyck language`` =
    let testName = "Second worst case, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>,0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>,0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>,0<terminalSymbol>, 0<inputGraphVertex>)
                                 
                                 TerminalEdge(0<inputGraphVertex>,1<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>,1<terminalSymbol>, 0<inputGraphVertex>) |])
        
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(29, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          nodes.Add(30, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,1<rsmState>))
          nodes.Add(42, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(43, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(44, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(45, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(46, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(48, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(49, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,0<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(50, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,0<inputGraphVertex>))
          nodes.Add(51, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(52, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(53, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(55, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(56, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(57, TriplesStoredSPPFNode.EpsilonNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(58, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(59, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(60, TriplesStoredSPPFNode.EpsilonNode (3<inputGraphVertex>,0<rsmState>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (1,3); (3,4); (4,5); (5,6); (6,7); (5,8); (8,9); (9,10); (10,11)
                                       (11,12); (12,13); (13,14); (14,15); (15,16); (16,17); (17,18); (18,19); (19,20)
                                       (20,21); (19,22); (22,23); (23,24); (24,25); (25,26); (26,27); (27,28); (28,29)
                                       (27,30); (30,0); (26,31); (31,32); (32,33); (33,34); (34,35); (35,36); (36,37)
                                       (37,28); (37,38); (38,39); (39,40); (40,41); (41,42); (42,43); (43,6); (43,44)
                                       (44,45); (45,16); (41,46); (46,47); (36,48); (48,32); (48,8); (35,49); (49,50)
                                       (33,6); (31,44); (25,46); (18,51); (51,14); (51,8); (17,49); (15,6); (13,44)
                                       (12,52); (52,20); (52,53); (53,54); (54,34); (11,46); (3,49); (55,56); (56,57)
                                       (58,59); (59,60)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Linear nested brackets, simple loop RSM for Dyck language`` =
    let testName = "Linear nested brackets, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 1<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>, 1<terminalSymbol>, 4<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.EpsilonNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.EpsilonNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(26, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,4<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (9,10); (10,11); (11,12); (12,13)
                                       (13,14); (14,15); (13,16); (16,6); (11,17); (17,18); (19,20); (20,21); (21,22)
                                       (22,23); (23,24); (24,25); (23,26); (26,9); (21,27); (27,28)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Complex nested linear brackets, simple loop RSM for Dyck language`` =
    let testName = "Complex nested linear brackets, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 1<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(3<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
                                 TerminalEdge(4<inputGraphVertex>, 1<terminalSymbol>, 5<inputGraphVertex>)
                                 TerminalEdge(5<inputGraphVertex>, 1<terminalSymbol>, 6<inputGraphVertex>)
                               |])
        let startV = [|0<inputGraphVertex>|]
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.EpsilonNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.EpsilonNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(10, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.EpsilonNode (4<inputGraphVertex>,0<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,5<inputGraphVertex>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,1<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,2<rsmState>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
          nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(31, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,5<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
          nodes.Add(34, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,6<inputGraphVertex>))
          nodes.Add(35, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(36, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,1<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(40, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(41, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,5<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(42, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,6<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(43, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (9,10); (10,11); (11,12); (12,13)
                                       (13,14); (14,15); (13,16); (16,6); (11,17); (17,18); (19,20); (20,21); (22,23)
                                       (23,24); (24,25); (25,26); (26,27); (27,28); (28,10); (28,29); (29,30); (26,31)
                                       (31,19); (24,32); (32,33); (34,35); (35,36); (36,37); (37,38); (38,39); (39,40)
                                       (38,41); (41,22); (36,42); (42,43)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected

let ``One edge linear graph, simple chain call RSM`` =
    let testName = "One edge linear graph, simple chain call RSM"
    testCase testName <| fun () ->
        let graph =
            InputGraph(
                [|
                    TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                |]
                )
        let startV = [|0<inputGraphVertex>|]
        let box2,m = makeRsmBox(Dictionary(),2<rsmState>, HashSet([3<rsmState>]),[|RSM.TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)|])
        let box1,_ = makeRsmBox(m,0<rsmState>, HashSet([1<rsmState>]),[|NonTerminalEdge(0<rsmState>,2<rsmState>,1<rsmState>)|])        
        let q = RSM([|box1; box2|], box1)
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,2<rsmState>,1<inputGraphVertex>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,3<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Two edges linear graph with one pair of brackets, simple loop RSM for Dyck language`` =
    let testName = "Two edges linear graph with one pair of brackets, simple loop RSM for Dyck language"
    testCase testName <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 2<inputGraphVertex>) |])
        let startV = [|0<inputGraphVertex>|]        
        let q = simpleLoopRSMForDyckLanguage ()
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.EpsilonNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,9); (9,10); (10,11); (11,12)
                                       (10,13); (13,3); (8,14); (14,15)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Graph with branch, RSM with nonterminal, nodes reusing`` =
    let testName = "Graph with branch, RSM with nonterminal, nodes reusing"
    testCase testName <| fun () ->
        let graph = InputGraph([|
            TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
            TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
            TerminalEdge(4<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
        |])
        let startV = [|0<inputGraphVertex>|]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([2<rsmState>]),
                         [|
                           RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                           RSM.NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                           RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,2<rsmState>)
                         |]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(11, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(12, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(13, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(15, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(19, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(23, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(25, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          nodes.Add(29, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(30, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(32, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(33, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(34, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(35, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(36, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(37, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
          nodes.Add(38, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,1<rsmState>))
          nodes.Add(39, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
          nodes.Add(40, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
          nodes.Add(41, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(43, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(44, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
          nodes.Add(45, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,1<rsmState>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,9); (9,10); (8,11); (11,3); (12,13)
                                       (13,14); (15,16); (16,17); (16,18); (18,19); (19,20); (18,21); (21,12); (22,23)
                                       (23,24); (24,9); (24,25); (25,15); (26,27); (27,28); (29,30); (30,31); (31,32)
                                       (32,33); (31,34); (34,26); (35,36); (36,37); (37,19); (37,38); (38,29); (36,39)
                                       (39,40); (40,41); (39,34); (42,43); (43,44); (44,9); (44,45); (45,35)
                                     |])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let ``Graph with branch, RSM is loop DFA, nodes reusing`` =
    let testName = "Graph with branch, RSM is loop DFA, nodes reusing"
    testCase testName <| fun () ->
        let graph = InputGraph([|
            TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
            TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
            TerminalEdge(1<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
            TerminalEdge(4<inputGraphVertex>, 0<terminalSymbol>, 2<inputGraphVertex>)
        |])
        let startV = [|0<inputGraphVertex>|]
        let box,_ = makeRsmBox(Dictionary(), 0<rsmState>, HashSet([0<rsmState>]),[|RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|]) 
        let q = RSM([|box|], box)
        
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
          nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
          nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
          nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
          nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
          nodes.Add(19, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
          nodes.Add(20, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(21, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
          nodes.Add(22, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(23, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
          
          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10); (11,12); (12,13)
                                       (13,4); (13,14); (14,15); (12,16); (16,7); (16,17); (17,18); (19,20); (20,21)
                                       (21,12); (21,22); (22,23)|])
          (nodes,edges)
        runGLLAndCheckResult testName graph startV q expected
        
let tests =  
    
  testList "GLL CFPQ Tests with SPPF" [
    ``Empty graph, Epsilon-only RSM``
    ``One edge linear graph, one edge linear RSM``
    ``One edge linear graph, one edge loop RSM``
    ``One edge loop graph, one edge loop RSM``
    ``Two edge linear graph, one edge loop RSM``
    ``Two concatenated linear pairs of brackets, simple loop RSM for Dyck language``
    ``Linear nested brackets, simple loop RSM for Dyck language``
    ``Complex nested linear brackets, simple loop RSM for Dyck language``
    ``Two loops with common vertex, simple loop RSM for Dyck language``
    ``One edge linear graph, simple chain call RSM``
    ``Two edges linear graph with one pair of brackets, simple loop RSM for Dyck language``
    ``Minimal worst case, simple loop RSM for Dyck language``
    ``Second worst case, simple loop RSM for Dyck language``
    ``Graph with branch, RSM with nonterminal, nodes reusing``
    ``Graph with branch, RSM is loop DFA, nodes reusing``
    ``Minimal example of interprocedural control flow``
  ] |> testSequenced

