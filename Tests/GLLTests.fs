module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests.InputGraph


(*let dumpResultToConsole (sppf:TriplesStoredSPPF<_>) =
    sppf.Edges |> Seq.iter (fun (x,y) -> printf $"(%i{x},%i{y}); ")
    printfn ""
    sppf.Nodes
    |> Seq.iter (fun kvp ->
        match kvp.Value with
        | TriplesStoredSPPFNode.EpsilonNode (_pos,_rsm, _w) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.EpsilonNode (%i{_pos}<inputGraphVertex>,%i{_rsm}<rsmState>, %i{_w}<distance>))"
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to, _w) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.TerminalNode (%i{_from}<inputGraphVertex>,%i{_terminal}<terminalSymbol>,%i{_to}<inputGraphVertex>, %i{_w}<distance>))"
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to, _w) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.NonTerminalNode (%i{_from}<inputGraphVertex>,%i{_nonTerminal}<rsmState>,%i{_to}<inputGraphVertex>, %i{_w}<distance>))"
        | TriplesStoredSPPFNode.RangeNode (_posFrom, _posTo, _rsmFrom, _rsmTo) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.RangeNode (%i{_posFrom}<inputGraphVertex>,%i{_posTo}<inputGraphVertex>,%i{_rsmFrom}<rsmState>,%i{_rsmTo}<rsmState>))"
        | TriplesStoredSPPFNode.IntermediateNode (_pos, _rsm, _w) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.IntermediateNode (%i{_pos}<inputGraphVertex>,%i{_rsm}<rsmState>, %i{_w}<distance>))"
        )
*)

let calculateActual root =

    let sppf = TriplesStoredSPPF([|root|], Dictionary())
    let countFor filter =
        sppf.Nodes
        |> Seq.filter (fun kvp -> filter kvp.Value)
        |> Seq.length

    let epsilonCount = countFor (function | TriplesStoredSPPFNode.EpsilonNode _ -> true | _ -> false)
    let terminalCount = countFor (function | TriplesStoredSPPFNode.TerminalNode _ -> true | _ -> false)
    let nonTerminalCount = countFor (function | TriplesStoredSPPFNode.NonTerminalNode _ -> true | _ -> false)
    let rangeCount = countFor (function | TriplesStoredSPPFNode.RangeNode _ -> true | _ -> false)
    let intermediateCount = countFor (function | TriplesStoredSPPFNode.IntermediateNode _ -> true | _ -> false)

    epsilonCount, terminalCount, nonTerminalCount, rangeCount, intermediateCount, root.Weight

let private runGLLAndCheckResultForManuallyCreatedGraph
    evalFunction
    (testName:string)
    (startVertex: LinearInputGraphVertexBase)
    (finalVertex: LinearInputGraphVertexBase)
    (q:RSM)
    (epsilonCountEx, terminalCountEx, nonTerminalCountEx, rangeCountEx, intermediateCountEx, weightEx) =

    let validDotFileName = testName.Replace(',', ' ').Replace(' ', '_') + ".dot"
    let result = evalFunction startVertex q AllPaths

    match result with
    | QueryResult.MatchedRanges ranges ->

        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> startVertex = n.LeftPosition && finalVertex = n.RightPosition) |> Array.minBy(fun n -> n.Weight)

        //printfn $"D for %s{validDotFileName}"

        //let actual = TriplesStoredSPPF([|root|], Dictionary())
        //actual.ToDot validDotFileName

        let epsilonCount, terminalCount, nonTerminalCount, rangeCount, intermediateCount, weight = calculateActual root
        Expect.equal epsilonCount epsilonCountEx "Epsilon nodes count should be equal"
        Expect.equal terminalCount terminalCountEx "Terminal nodes count should be equal"
        Expect.equal nonTerminalCount nonTerminalCountEx "NonTerminal nodes count should be equal"
        Expect.equal rangeCount rangeCountEx "Range nodes count should be equal"
        Expect.equal intermediateCount intermediateCountEx "Intermediate nodes count should be equal"
        Expect.equal weight weightEx "Weights should be equal"

    | _ -> failwith "Result should be MatchedRanges"


let runErrorRecoveringGLLAndCheckResult (testName:string) (graph:InputGraph) startV finalV (q:RSM) (epsilonCountEx, terminalCountEx, nonTerminalCountEx, rangeCountEx, intermediateCountEx, weightEx) =
    let startVertex,mapping = graph.ToCfpqCoreGraph startV
    let finalVertex = mapping[finalV]
    runGLLAndCheckResultForManuallyCreatedGraph (errorRecoveringEval finalVertex) testName startVertex finalVertex q (epsilonCountEx, terminalCountEx, nonTerminalCountEx, rangeCountEx, intermediateCountEx, weightEx)


