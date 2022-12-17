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

let fillRsmBox (box:RSMBox, statesMapping: Dictionary<int<rsmState>, IRsmState>, startSate:int<rsmState>, finalStates: HashSet<int<rsmState>>, edges: array<RSMEdges>) =

    let getState stateId =
        let exists, state = statesMapping.TryGetValue stateId
        if exists
        then state
        else
            let state = RsmState((stateId = startSate), finalStates.Contains stateId)
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
            startState.AddNonTerminalEdge(nonTerm, finalState)
        | RSMEdges.TerminalEdge (_from,_term,_to ) ->
            startState.AddTerminalEdge(_term, finalState)
    statesMapping

let makeRsmBox (statesMapping: Dictionary<int<rsmState>, IRsmState>, startSate:int<rsmState>, finalStates: HashSet<int<rsmState>>, edges: array<RSMEdges>) =
    let box = RSMBox()
    let statesMapping = fillRsmBox (box, statesMapping, startSate, finalStates, edges)
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

let private runGLLAndCheckResultForManuallyCreatedGraph
    evalFunction
    (testName:string)
    (startVertex: ILinearInputGraphVertex)
    (finalVertex: ILinearInputGraphVertex)
    (q:RSM)
    (expectedNodes, expectedEdges, expectedDistances) =

    let validDotFileName = testName.Replace(',', ' ').Replace(' ', '_') + ".dot"
    let result = evalFunction startVertex q AllPaths
    match result with
    | QueryResult.MatchedRanges ranges ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> startVertex = n.LeftPosition && finalVertex = n.RightPosition) |> Array.minBy(fun n -> n.Distance)
        let distances = [|root.Distance|]
        printfn $"D for %s{validDotFileName}: %A{distances}"
        let actual = TriplesStoredSPPF([|root|], Dictionary())
        dumpResultToConsole actual
        actual.ToDot (validDotFileName)
        Expect.sequenceEqual actual.Nodes expectedNodes "Nodes should be equals."
        Expect.sequenceEqual actual.Edges expectedEdges "Edges should be equals."
        Expect.sequenceEqual distances expectedDistances "Distances should be equals."

    | _ -> failwith "Result should be MatchedRanges"


let runErrorRecoveringGLLAndCheckResult (testName:string) (graph:InputGraph) startV finalV (q:RSM) (expectedNodes, expectedEdges, expectedDistances) =
    let startVertex,mapping = graph.ToCfpqCoreGraph startV
    let finalVertex = mapping[finalV]
    runGLLAndCheckResultForManuallyCreatedGraph (errorRecoveringEval finalVertex) testName startVertex finalVertex q (expectedNodes, expectedEdges, expectedDistances)


