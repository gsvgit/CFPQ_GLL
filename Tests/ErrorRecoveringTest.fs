module Tests.ErrorRecoveringTest


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
open Tests.LinearGraphReader

let test2 (graph: InputGraph) q =

    let startV = [|0<inputGraphVertex>|]
    let finalV = [|LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)|]
    graph.ToDot (0, "graph.dot")
    let startVertices,mapping = graph.ToCfpqCoreGraph (HashSet startV)
    let finalVertices = Array.map (fun x -> mapping[x]) finalV |> HashSet
    let result = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths

    match result with
    | GLL.QueryResult.MatchedRanges ranges ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> finalVertices.Contains n.RightPosition && startVertices.Contains n.LeftPosition) |> Array.minBy(fun n -> n.Distance)
        let distances = sppf |> Array.map (fun n -> n.Distance) |> Array.sort
        //printfn $"D for %s{validDotFileName}: %A{distances}"
        let actual = TriplesStoredSPPF([|root|], Dictionary())

        actual.ToDot "sppf.dot"

        Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0

let testFromString (maker: Config -> string -> InputGraph) text mode q =
    test2 (maker text mode) q

let ``Error recovering tests`` =

    let ``On ab RSM`` =

        let abRSM () =
            // S -> ab
            let startState = RsmState(true, false) :> IRsmState
            let state = RsmState(false, false) :> IRsmState
            let finalState = RsmState(false, true) :> IRsmState
            let box = RSMBox()
            box.AddState startState
            box.AddState state
            box.AddState finalState
            startState.OutgoingTerminalEdges.Add(1<terminalSymbol>, HashSet[|state|])
            state.OutgoingTerminalEdges.Add(2<terminalSymbol>, HashSet[finalState])
            RSM([|box|], box)

        let abGraphMaker = mkLinearGraph id (
            Dictionary([KeyValuePair('a',1<terminalSymbol>); KeyValuePair('b', 2<terminalSymbol>)])
        )

        let ``Only deletions`` =

            let graphMaker = abGraphMaker Config.LinearGraphWithDeletions

            let ab =
                let testName = "ab"
                testCase testName <| fun () ->
                    let graph = graphMaker "ab"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|2<inputGraphVertex>|]
                    let q = abRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()
                        nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                        nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                        nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                        nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))

                        let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6); |])
                        let distances = [|0<distance>|]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            let abb =
                let testName = "abb"
                testCase testName <| fun () ->
                    let graph = graphMaker "abb"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|3<inputGraphVertex>|]
                    let q = abRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()
                        nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                        nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                        nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                        nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
                        nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
                        nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                        nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,1<inputGraphVertex>))


                        let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10); |])
                        let distances = [|1<distance>|]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            testList "Only deletions" [
                ab
                abb
            ] |> testSequenced

        let ``Deletions and insertions`` =

            let graphMaker = abGraphMaker Config.LinearGraphWithDeletionsAndInsertions

            let ab =
                let testName = "ab"
                testCase testName <| fun () ->
                    let graph = graphMaker "ab"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|2<inputGraphVertex>|]
                    let q = abRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()
                        nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                        nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                        nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                        nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))

                        let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6);|])
                        let distances = [|0<distance>|]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            let abb =
                let testName = "abb"
                testCase testName <| fun () ->
                    let graph = graphMaker "abb"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|3<inputGraphVertex>|]
                    let q = abRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()
                        nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                        nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                        nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                        nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
                        nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
                        nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                        nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,1<inputGraphVertex>))


                        let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10); |])
                        let distances = [|1<distance>|]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            let bb =
                let testName = "bb"
                testCase testName <| fun () ->
                    let graph = graphMaker "bb"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|2<inputGraphVertex>|]
                    let q = abRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()
                        nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                        nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                        nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                        nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
                        nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,0<inputGraphVertex>))
                        nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
                        nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                        nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,1<inputGraphVertex>))
                        nodes.Add(11, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                        nodes.Add(12, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(13, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
                        nodes.Add(14, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                        nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                        nodes.Add(17, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
                        nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(19, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                        nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                        nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                        nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))

                        let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9)
                                                     (9,10); (1,11); (11,12); (12,13); (13,5); (13,14); (14,15); (12,16)
                                                     (16,17); (17,18); (16,19); (19,20); (11,21); (21,22); |])
                        let distances = [|2<distance>|]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            testList "Deletions and insertions" [
                ab
                abb
                bb
            ] |> testSequenced

        testList "On ab RSM" [
            ``Only deletions``
            ``Deletions and insertions``
        ] |> testSequenced

    let ``On ca*b* RSM`` =

        let CAStarBStarRSM () =
            // S -> ca*b*
            let startState = RsmState(true, false) :> IRsmState
            let aState = RsmState(false, true) :> IRsmState
            let bState = RsmState(false, true) :> IRsmState
            let box = RSMBox()
            box.AddState startState
            box.AddState aState
            box.AddState bState
            startState.OutgoingTerminalEdges.Add(3<terminalSymbol>, HashSet[|aState|])
            aState.OutgoingTerminalEdges.Add(1<terminalSymbol>, HashSet[|aState|])
            aState.OutgoingTerminalEdges.Add(2<terminalSymbol>, HashSet[|bState|])
            bState.OutgoingTerminalEdges.Add(2<terminalSymbol>, HashSet[|bState|])
            RSM([|box|], box)

        let abGraphMaker = mkLinearGraph id (
            Dictionary([KeyValuePair('a',1<terminalSymbol>); KeyValuePair('b', 2<terminalSymbol>); KeyValuePair('c', 3<terminalSymbol>)])
        )

        let ``Only deletions`` =

            let graphMaker = abGraphMaker Config.LinearGraphWithDeletions

            let cab =
                let testName = "cab"
                testCase testName <| fun () ->
                    let graph = graphMaker "cab"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|3<inputGraphVertex>|]
                    let q = CAStarBStarRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()

                        let edges = ResizeArray<_>([||])
                        let distances = [||]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            testList "Only deletions" [

            ] |> testSequenced

        let ``Deletions and insertions`` =

            let graphMaker = abGraphMaker Config.LinearGraphWithDeletionsAndInsertions

            let test =
                let testName = "bb"
                testCase testName <| fun () ->
                    let graph = graphMaker "bb"
                    let startV = [|0<inputGraphVertex>|]
                    let finishV = [|2<inputGraphVertex>|]
                    let q = CAStarBStarRSM ()
                    let expected =
                        let nodes = Dictionary<_,_>()


                        let edges = ResizeArray<_>([||])
                        let distances = [||]
                        (nodes,edges,distances)

                    runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

            testList "Deletions and insertions" [

            ] |> testSequenced

        testList "On ab RSM" [
            ``Only deletions``
            ``Deletions and insertions``
        ] |> testSequenced

    testList "Error recovering tests" [
        ``On ab RSM``
        ``On ca*b* RSM``
    ] |> testSequenced

