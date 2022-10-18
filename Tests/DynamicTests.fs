module Tests.DynamicTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests.InputGraph

let checkResult (testName:string) startVertices (q:RSM) (expectedNodes, expectedEdges, expectedDistances) result =
    let validDotFileName = testName.Replace(',', ' ').Replace(' ', '_') + ".dot"
    match result with
    | QueryResult.MatchedRanges ranges ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let distances = sppf |> Array.map (fun n -> n.Distance) |> Array.sort
        printfn $"D for %s{validDotFileName}: %A{distances}"
        let actual = TriplesStoredSPPF(sppf, Dictionary())
        GLLTests.dumpResultToConsole actual
        actual.ToDot validDotFileName
        Expect.sequenceEqual actual.Nodes expectedNodes "Nodes should be equals."
        Expect.sequenceEqual actual.Edges expectedEdges "Edges should be equals."
        Expect.sequenceEqual distances expectedDistances "Distances should be equals."
    | _ -> failwith "Result should be MatchedRanges"


let runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) (testName:string) startVertices (q:RSM) (expectedNodes, expectedEdges, expectedDistances) =
    let result = defaultEvalFromState reachableVertices gss matchedRanges startVertices (HashSet()) q AllPaths
    checkResult (testName:string) startVertices (q:RSM) (expectedNodes, expectedEdges, expectedDistances) result

let ``Simple call`` =
    let testName = "Simple call"
    testCase testName <| fun () ->
        let terminalForCFGEdge = 0<terminalSymbol>
        let graphEntryPoint = InputGraphVertexBase() :> IInputGraphVertex
        let graphExit = InputGraphVertexBase() :> IInputGraphVertex
        graphEntryPoint.OutgoingEdges.Add(terminalForCFGEdge, HashSet [|TerminalEdgeTarget graphExit|])

        let balancedBracketsRsmBoxStartState = RsmState(true,true) :> IRsmState
        let balancedBracketsRsmBoxFinalState = balancedBracketsRsmBoxStartState
        let callImbalanceRsmBoxStartState = RsmState(true,false) :> IRsmState
        let callImbalanceRsmBoxFinalState = RsmState(false,true) :> IRsmState
        let historyRsmBoxStartState = RsmState(true,false) :> IRsmState
        let historyRsmBoxDefaultFinalState = RsmState(false,true) :> IRsmState
        let historyRsmBoxHistoryStartState = RsmState(false,true) :> IRsmState

        let historyRsmBox =
            let box = RSMBox()
            box.AddState historyRsmBoxStartState
            box.AddState historyRsmBoxDefaultFinalState
            box.AddState historyRsmBoxHistoryStartState
            historyRsmBoxStartState.OutgoingNonTerminalEdges.Add(callImbalanceRsmBoxStartState, HashSet [|historyRsmBoxDefaultFinalState|])
            historyRsmBoxStartState.OutgoingNonTerminalEdges.Add(balancedBracketsRsmBoxStartState, HashSet [|historyRsmBoxHistoryStartState|])
            box
        let balancedBracketsRsmBox =
            let box = RSMBox()
            box.AddState balancedBracketsRsmBoxStartState
            balancedBracketsRsmBoxStartState.OutgoingTerminalEdges.Add(terminalForCFGEdge, HashSet [|balancedBracketsRsmBoxFinalState|])
            box
        let callImbalanceBracketsRsmBox =
            let box = RSMBox()
            box.AddState callImbalanceRsmBoxStartState
            box.AddState callImbalanceRsmBoxFinalState
            callImbalanceRsmBoxStartState.OutgoingNonTerminalEdges.Add(balancedBracketsRsmBoxStartState, HashSet [|callImbalanceRsmBoxFinalState|])
            box

        let q = RSM([|historyRsmBox; balancedBracketsRsmBox; callImbalanceBracketsRsmBox|], historyRsmBox)

        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,2<rsmState>,1<inputGraphVertex>))
          nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,2<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(5, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(6, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,1<rsmState>))
          nodes.Add(7, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,2<rsmState>,0<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,2<rsmState>,2<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,2<rsmState>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (5,6); (6,7); (7,8); (8,9)|])
          let distances = [|0<distance>; 1<distance>|]
          (nodes,edges,distances)

        let startVertices = (HashSet [|graphEntryPoint|])

        let reachableVertices, gss, matchedRanges =
            Dictionary<_,_>(),
            GSS(),
            MatchedRanges()

        //runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected

        //MatchedRanges.Invalidate graphExit.TerminalNodes.[graphEntryPoint].[terminalForCFGEdge]
        graphEntryPoint.OutgoingEdges.Clear()
        let secondMethodEntryPoint = InputGraphVertexBase () :> IInputGraphVertex
        let secondMethodExit = InputGraphVertexBase () :> IInputGraphVertex
        secondMethodEntryPoint.OutgoingEdges.Add(terminalForCFGEdge, HashSet [|TerminalEdgeTarget secondMethodExit|])
        graphEntryPoint.OutgoingEdges.Add(1<terminalSymbol>, HashSet [|TerminalEdgeTarget secondMethodEntryPoint|])
        secondMethodExit.OutgoingEdges.Add(2<terminalSymbol>, HashSet [|TerminalEdgeTarget graphExit|])
        let verticesWithChanges = HashSet [|graphEntryPoint|]

        callImbalanceRsmBoxFinalState.OutgoingTerminalEdges.Add(1<terminalSymbol>, HashSet [|callImbalanceRsmBoxStartState|])

        let newRsmState1 = RsmState() :> IRsmState
        let newRsmState2 = RsmState() :> IRsmState
        balancedBracketsRsmBox.AddState newRsmState1
        balancedBracketsRsmBox.AddState newRsmState2
        balancedBracketsRsmBoxStartState.OutgoingTerminalEdges.Add(1<terminalSymbol>, HashSet [|newRsmState1|])
        newRsmState1.OutgoingNonTerminalEdges.Add(balancedBracketsRsmBoxStartState, HashSet [|newRsmState2|])
        newRsmState2.OutgoingTerminalEdges.Add(2<terminalSymbol>, HashSet[|balancedBracketsRsmBoxFinalState|])

        let newRsmState = RsmState() :> IRsmState
        historyRsmBox.AddState newRsmState
        historyRsmBoxHistoryStartState.OutgoingTerminalEdges.Add(2<terminalSymbol>, HashSet [|newRsmState|])
        newRsmState.OutgoingNonTerminalEdges.Add(balancedBracketsRsmBoxStartState, HashSet [|historyRsmBoxHistoryStartState|])

        q.ToDot "rsm_d.dot"
        runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected
        //let result = onInputGraphChanged verticesWithChanges reachableVertices gss matchedRanges startVertices q Mode.AllPaths
        //checkResult (testName:string) startVertices (q:RSM) expected result

let tests =
  testList "GLL tests on graph changes" [
    ``Simple call``
  ]
