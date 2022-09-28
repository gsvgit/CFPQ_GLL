module Tests.EpsilonEdge

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open CFPQ_GLL
open Expecto
open FSharpx.Collections
open GLLTests
open Tests.InputGraph
let ``Epsilon edges tests`` =

    let character = [|for i in [0..26] do LanguagePrimitives.Int32WithMeasure<terminalSymbol> i|]

    let ``Epsilon edges tests on (S -> aa) RSM`` =

        let aaRSM () =
            // S -> aa
            let startState = RsmState(true, false) :> IRsmState
            let state = RsmState(false, false) :> IRsmState
            let finalState = RsmState(false, true) :> IRsmState
            let box = RSMBox()
            box.AddState startState
            box.AddState state
            box.AddState finalState
            startState.OutgoingTerminalEdges.Add(character[0], HashSet[|state|])
            state.OutgoingTerminalEdges.Add(character[0], HashSet[finalState])
            RSM([|box|], box)

        let ``a epsilon a`` =
            let testName = "a epsilon a"
            testCase testName <| fun () ->
                let graph = [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                               EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                               TerminalEdge(2<inputGraphVertex>, character[0], 3<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aaRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10);|])
                    let distances = [|3<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``a epsilon epsilon a`` =
            let testName = "a epsilon epsilon a"
            testCase testName <| fun () ->
                let graph = [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                               EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                               EpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
                               TerminalEdge(3<inputGraphVertex>, character[0], 4<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aaRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,2<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8)
                                                 (6,9); (9,10); (4,11); (11,12); (2,13); (13,14);|])
                    let distances = [|4<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``a b|epsilon a`` =
            let testName = "a b|epsilon a"
            testCase testName <| fun () ->
                let graph = [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                               TerminalEdge(1<inputGraphVertex>, character[1], 2<inputGraphVertex>)
                               EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                               TerminalEdge(2<inputGraphVertex>, character[0], 3<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aaRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10);|])
                    let distances = [|3<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``a b|epsilon c|epsilon a`` =
            let testName = "a b|epsilon c|epsilon a"
            testCase testName <| fun () ->
                let graph =  [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                                EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                                TerminalEdge(1<inputGraphVertex>, character[1], 2<inputGraphVertex>)
                                EpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
                                TerminalEdge(2<inputGraphVertex>, character[2], 3<inputGraphVertex>)
                                TerminalEdge(3<inputGraphVertex>, character[0], 4<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aaRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,2<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8)
                                                 (6,9); (9,10); (4,11); (11,12); (2,13); (13,14);|])
                    let distances = [|4<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``epsilon a epsilon a epsilon`` =
            let testName = "epsilon a epsilon a epsilon"
            testCase testName <| fun () ->
                let graph = [| EpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
                               TerminalEdge(1<inputGraphVertex>, character[0], 2<inputGraphVertex>)
                               EpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
                               TerminalEdge(3<inputGraphVertex>, character[0], 4<inputGraphVertex>)
                               EpsilonEdge(4<inputGraphVertex>, 5<inputGraphVertex>)|] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aaRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
                    nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,4<inputGraphVertex>,1<rsmState>,1<rsmState>))
                    nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,-1<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,0<rsmState>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,-1<terminalSymbol>,0<inputGraphVertex>))
                    nodes.Add(25, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
                    nodes.Add(26, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(27, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))


                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10)
                                                 (11,12); (12,13); (13,1); (13,14); (14,15); (16,17); (17,18); (18,19)
                                                 (19,20); (20,21); (21,22); (22,23); (23,24); (22,5); (20,7); (18,9)
                                                 (25,26); (26,27); (27,17); (27,14); |])
                    let distances = [|3<distance>; 4<distance>; 4<distance>; 5<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        testList "Epsilon edges tests on aa RSM" [
            ``a epsilon a``
            ``a epsilon epsilon a``
            ``a b|epsilon a``
            ``a b|epsilon c|epsilon a``
            ``epsilon a epsilon a epsilon``
        ] |> testSequenced

    let ``Epsilon edges tests on (S -> a*) RSM`` =

        let aStarRSM () =
            // S -> a*
            let state = RsmState(true, true) :> IRsmState
            let box = RSMBox()
            box.AddState state
            state.OutgoingTerminalEdges.Add(character[0], HashSet[|state|])
            RSM([|box|], box)

        let ``a epsilon a`` =
            let testName = "a epsilon a"
            testCase testName <| fun () ->
                let graph = [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                               EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                               TerminalEdge(2<inputGraphVertex>, character[0], 3<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aStarRSM ()
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
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
                    nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))


                    let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10); (11,12)
                                                 (12,13); (13,7); (13,14); (14,15);|])
                    let distances = [|0<distance>; 1<distance>; 2<distance>; 3<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``a epsilon epsilon a`` =
            let testName = "a epsilon epsilon a"
            testCase testName <| fun () ->
                let graph = [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                               EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                               EpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
                               TerminalEdge(3<inputGraphVertex>, character[0], 4<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aStarRSM ()
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
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
                    nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))


                    let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10); (11,12)
                                                 (12,13); (13,7); (13,14); (14,15); (16,17); (17,18); (18,12); (18,19)
                                                 (19,20);|])
                    let distances = [|0<distance>; 1<distance>; 2<distance>; 3<distance>; 4<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``a b|epsilon a`` =
            let testName = "a b|epsilon a"
            testCase testName <| fun () ->
                let graph = [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                               TerminalEdge(1<inputGraphVertex>, character[1], 2<inputGraphVertex>)
                               EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                               TerminalEdge(2<inputGraphVertex>, character[0], 3<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aStarRSM ()
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
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
                    nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))


                    let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10); (11,12)
                                                 (12,13); (13,7); (13,14); (14,15); |])
                    let distances = [|0<distance>; 1<distance>; 2<distance>; 3<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``a b|epsilon c|epsilon a`` =
            let testName = "a b|epsilon c|epsilon a"
            testCase testName <| fun () ->
                let graph =  [| TerminalEdge(0<inputGraphVertex>, character[0], 1<inputGraphVertex>)
                                EpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                                TerminalEdge(1<inputGraphVertex>, character[1], 2<inputGraphVertex>)
                                EpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
                                TerminalEdge(2<inputGraphVertex>, character[2], 3<inputGraphVertex>)
                                TerminalEdge(3<inputGraphVertex>, character[0], 4<inputGraphVertex>) |] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aStarRSM ()
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
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
                    nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,4<inputGraphVertex>))


                    let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10); (11,12)
                                                 (12,13); (13,7); (13,14); (14,15); (16,17); (17,18); (18,12); (18,19)
                                                 (19,20);|])
                    let distances =  [|0<distance>; 1<distance>; 2<distance>; 3<distance>; 4<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        let ``epsilon a epsilon a epsilon`` =
            let testName = "epsilon a epsilon a epsilon"
            testCase testName <| fun () ->
                let graph = [| EpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
                               TerminalEdge(1<inputGraphVertex>, character[0], 2<inputGraphVertex>)
                               EpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
                               TerminalEdge(3<inputGraphVertex>, character[0], 4<inputGraphVertex>)
                               EpsilonEdge(4<inputGraphVertex>, 5<inputGraphVertex>)|] |> InputGraph
                let startV = [|0<inputGraphVertex>|]
                let q = aStarRSM ()
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
                    nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
                    nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(21, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,5<inputGraphVertex>))
                    nodes.Add(22, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(23, TriplesStoredSPPFNode.EpsilonNode (5<inputGraphVertex>,0<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,-1<terminalSymbol>,0<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(28, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(29, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,0<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
                    nodes.Add(31, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,0<rsmState>))
                    nodes.Add(33, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,3<inputGraphVertex>))
                    nodes.Add(34, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,0<rsmState>,4<inputGraphVertex>))
                    nodes.Add(37, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,0<rsmState>))
                    nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))



                    let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10); (11,12)
                                                 (12,13); (13,7); (13,14); (14,15); (16,17); (17,18); (18,12); (18,19)
                                                 (19,20); (21,22); (22,23); (24,25); (25,26); (27,28); (28,29); (29,25)
                                                 (29,4); (30,31); (31,32); (32,28); (32,9); (33,34); (34,35); (35,31)
                                                 (35,14); (36,37); (37,38); (38,34); (38,19);  |])
                    let distances = [|0<distance>; 0<distance>; 1<distance>; 1<distance>
                                      2<distance>; 2<distance>; 3<distance>; 3<distance>
                                      4<distance>; 4<distance>; 5<distance>|]
                    (nodes,edges,distances)
                runGLLAndCheckResult testName graph startV q expected

        testList "Epsilon edges tests on a* RSM" [
            ``a epsilon a``
            ``a epsilon epsilon a``
            ``a b|epsilon a``
            ``a b|epsilon c|epsilon a``
            ``epsilon a epsilon a epsilon``
        ] |> testSequenced

    testList "Epsilon edges test" [
        ``Epsilon edges tests on (S -> aa) RSM``
        ``Epsilon edges tests on (S -> a*) RSM``
    ] |> testSequenced
