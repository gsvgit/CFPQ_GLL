module Tests.DynamicTests.A_or_B_Star

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
open CFPQ_GLL.RsmBuilder
open Tests

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
    let result = evalFromState reachableVertices gss matchedRanges startVertices q AllPaths
    checkResult (testName:string) startVertices (q:RSM) (expectedNodes, expectedEdges, expectedDistances) result

let terminalA = 0<terminalSymbol>
let terminalB = 1<terminalSymbol>

let initialGraph () =
    let graphEntryPoint = InputGraphVertexBase() :> IInputGraphVertex
    let graphExit = InputGraphVertexBase() :> IInputGraphVertex
    graphEntryPoint.OutgoingEdges.Add(terminalA, HashSet [|graphExit|])
    graphEntryPoint, graphExit

let initialTwoEdgesGraph () =
    let v0 = InputGraphVertexBase() :> IInputGraphVertex
    let v1 = InputGraphVertexBase() :> IInputGraphVertex
    let v2 = InputGraphVertexBase() :> IInputGraphVertex
    v0.OutgoingEdges.Add(terminalA, HashSet [|v1|])
    v1.OutgoingEdges.Add(terminalA, HashSet [|v2|])
    v0,v1,v2

let rsm () =
    // Language grammar
    // S -> (a|b)*

    let S = nt "S"

    [
        S   =>  many(t "a" *|* t "b")
    ] |> build

let expectedForInitialOneEdgeGraph =
      let nodes = Dictionary<_,_>()
      nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
      nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
      nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
      nodes.Add(3, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
      nodes.Add(4, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
      nodes.Add(5, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

      let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5)|])
      let distances = [|0<distance>; 1<distance>|]
      (nodes,edges,distances)

let expectedForInitialTwoEdgesGraph =
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
      let distances = [|0<distance>; 1<distance> ; 2<distance>|]
      (nodes,edges,distances)


let ``(a|b)* add A to end`` =
    let testName = "(a|b)* add A to end"
    testCase testName <| fun () ->

        let graphEntryPoint, graphExit = initialGraph()

        let q,_ = rsm()



        let startVertices = (HashSet [|graphEntryPoint|])

        let reachableVertices, gss, matchedRanges =
            Dictionary<_,_>(),
            GSS(),
            MatchedRanges()

        let expected = expectedForInitialOneEdgeGraph
        runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected
        let newGraphExit = InputGraphVertexBase() :> IInputGraphVertex
        graphExit.OutgoingEdges.Add(terminalA, HashSet [|newGraphExit|])

        let verticesWithChanges = HashSet [|graphExit|]


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
          let distances = [|0<distance>; 1<distance>; 2<distance>|]
          (nodes,edges,distances)

        let result = onInputGraphChanged verticesWithChanges reachableVertices gss matchedRanges startVertices q Mode.AllPaths
        checkResult (testName:string) startVertices (q:RSM) expected result

let ``(a|b)* add B to end`` =
    let testName = "(a|b)* add B to end"
    testCase testName <| fun () ->

        let graphEntryPoint, graphExit = initialGraph()

        let q,_ = rsm()

        let expected = expectedForInitialOneEdgeGraph

        let startVertices = (HashSet [|graphEntryPoint|])

        let reachableVertices, gss, matchedRanges =
            Dictionary<_,_>(),
            GSS(),
            MatchedRanges()

        runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected
        let newGraphExit = InputGraphVertexBase() :> IInputGraphVertex
        graphExit.OutgoingEdges.Add(terminalB, HashSet [|newGraphExit|])

        let verticesWithChanges = HashSet [|graphExit|]


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
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10)|])
          let distances = [|0<distance>; 1<distance>; 2<distance>|]
          (nodes,edges,distances)

        let result = onInputGraphChanged verticesWithChanges reachableVertices gss matchedRanges startVertices q Mode.AllPaths
        checkResult (testName:string) startVertices (q:RSM) expected result

let ``(a|b)* add branch with B to start`` =
    let testName = "(a|b)* add branch with B to start"
    testCase testName <| fun () ->

        let graphEntryPoint, graphExit = initialGraph()

        let q,_ = rsm()

        let expected = expectedForInitialOneEdgeGraph

        let startVertices = (HashSet [|graphEntryPoint|])

        let reachableVertices, gss, matchedRanges =
            Dictionary<_,_>(),
            GSS(),
            MatchedRanges()

        runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected
        let newGraphExit = InputGraphVertexBase() :> IInputGraphVertex
        graphEntryPoint.OutgoingEdges.Add(terminalB, HashSet [|newGraphExit|])

        let verticesWithChanges = HashSet [|graphEntryPoint|]


        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
          nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))
          nodes.Add(7, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,2<inputGraphVertex>))
          nodes.Add(8, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(9, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (1,3); (4,5); (5,6); (7,8); (8,9)|])
          let distances = [|0<distance>; 1<distance>; 1<distance>|]
          (nodes,edges,distances)

        let result = onInputGraphChanged verticesWithChanges reachableVertices gss matchedRanges startVertices q Mode.AllPaths

        checkResult (testName:string) startVertices (q:RSM) expected result

let ``(a|b)* replace last A with B`` =
    let testName = "(a|b)* replace last A with B"
    testCase testName <| fun () ->

        let v0,v1,v2 = initialTwoEdgesGraph()

        let q,_ = rsm()


        let startVertices = (HashSet [|v0|])

        let reachableVertices, gss, matchedRanges =
            Dictionary<_,_>(),
            GSS(),
            MatchedRanges()

        let expected = expectedForInitialTwoEdgesGraph
        runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected

        v1.OutgoingEdges.Clear()
        MatchedRanges.Invalidate v2.TerminalNodes.[v1].[terminalA]
        v1.OutgoingEdges.Add(terminalB, HashSet [|v2|])

        let verticesWithChanges = HashSet [|v1|]

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
          nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))

          let edges = ResizeArray<_>([|(0,1); (1,2); (3,4); (4,5); (6,7); (7,8); (8,4); (8,9); (9,10)|])
          let distances = [|0<distance>; 1<distance>; 2<distance>|]
          (nodes,edges,distances)

        let result = onInputGraphChanged verticesWithChanges reachableVertices gss matchedRanges startVertices q Mode.AllPaths
        checkResult (testName:string) startVertices (q:RSM) expected result


let ``(a|b)* replace first A with B`` =
    let testName = "(a|b)* replace first A with B"
    testCase testName <| fun () ->

        let v0,v1,v2 = initialTwoEdgesGraph()

        let q,_ = rsm()


        let startVertices = (HashSet [|v0|])

        let reachableVertices, gss, matchedRanges =
            Dictionary<_,_>(),
            GSS(),
            MatchedRanges()

        let expected = expectedForInitialTwoEdgesGraph
        runGLLAndCheckResultForManuallyCreatedGraph (reachableVertices, gss, matchedRanges) testName startVertices q expected

        v0.OutgoingEdges.Clear()
        MatchedRanges.Invalidate v1.TerminalNodes.[v0].[terminalA]
        v0.OutgoingEdges.Add(terminalB, HashSet [|v1|])

        let verticesWithChanges = HashSet [|v0|]

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
          let distances = [|0<distance>; 1<distance>; 2<distance>|]
          (nodes,edges,distances)

        ()
        let result = onInputGraphChanged verticesWithChanges reachableVertices gss matchedRanges startVertices q Mode.AllPaths
        checkResult (testName:string) startVertices (q:RSM) expected result


let tests =
  testList "(a|b)* dynamic tests" [
  //  ``(a|b)* replace first A with B``
    ``(a|b)* replace last A with B``
    ``(a|b)* add A to end``
    ``(a|b)* add B to end``
    ``(a|b)* add branch with B to start``
  ]
