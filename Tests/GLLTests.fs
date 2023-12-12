module Tests.GLLTests

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests.InputGraph
open CFPQ_GLL.LinearInputGraph


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
    (startVertex: LinearInputGraphVertexBase<_>)
    (finalVertex: LinearInputGraphVertexBase<_>)
    (q:RSM<_>)
    (epsilonCountEx, terminalCountEx, nonTerminalCountEx, rangeCountEx, intermediateCountEx, weightEx) =

    let result, _ = evalFunction startVertex q AllPaths Epsilon

    match result with
    | QueryResult.MatchedRanges _ ->

        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> (startVertex :> IInputGraphVertex<_>) = n.LeftPosition && (finalVertex :> IInputGraphVertex<_>) = n.RightPosition) |> Array.minBy(fun n -> n.Weight)

        let epsilonCount, terminalCount, nonTerminalCount, rangeCount, intermediateCount, weight = calculateActual root
        Expect.equal epsilonCount epsilonCountEx "Epsilon nodes count should be equal"
        Expect.equal terminalCount terminalCountEx "Terminal nodes count should be equal"
        Expect.equal nonTerminalCount nonTerminalCountEx "NonTerminal nodes count should be equal"
        Expect.equal rangeCount rangeCountEx "Range nodes count should be equal"
        Expect.equal intermediateCount intermediateCountEx "Intermediate nodes count should be equal"
        Expect.equal weight weightEx "Weights should be equal"

    | _ -> failwith "Result should be MatchedRanges"


let runErrorRecoveringGLLAndCheckResult (graph:InputGraph) startV finalV (q:RSM<_>) (epsilonCountEx, terminalCountEx, nonTerminalCountEx, rangeCountEx, intermediateCountEx, weightEx) =
    let startVertex,(mapping:Dictionary<_,_>) = graph.ToCfpqCoreGraph startV
    let finalVertex = mapping[finalV]
    runGLLAndCheckResultForManuallyCreatedGraph (errorRecoveringEval finalVertex) startVertex finalVertex q (epsilonCountEx, terminalCountEx, nonTerminalCountEx, rangeCountEx, intermediateCountEx, weightEx)


