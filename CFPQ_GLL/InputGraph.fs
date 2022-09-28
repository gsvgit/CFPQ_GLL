module CFPQ_GLL.InputGraph

open System.Collections.Generic
open CFPQ_GLL.Common

[<Measure>] type inputGraphVertex

type InputGraphVertexBase () =
    let outgoingEdges = Dictionary<int<terminalSymbol>, HashSet<IInputGraphVertex>>()
    let descriptors = HashSet<Descriptor>()
    let terminalNodes = Dictionary<IInputGraphVertex, Dictionary<int<terminalSymbol>, ITerminalNode>>()
    let nonTerminalNodes = Dictionary<IInputGraphVertex, Dictionary<IRsmState, INonTerminalNode>>()
    let rangeNodes = Dictionary<MatchedRange, IRangeNode>()
    let intermediateNodes = Dictionary<MatchedRange, Dictionary<MatchedRange, IIntermediateNode>>()
    //let nonTerminalsWithStartHere = HashSet<_>()
    interface IInputGraphVertex with
        member this.OutgoingEdges = outgoingEdges
        member this.Descriptors = descriptors
        member this.TerminalNodes = terminalNodes
        member this.NonTerminalNodesStartedHere = nonTerminalNodes
        //member this.NonTerminalNodesWithStartHere = nonTerminalsWithStartHere
        member this.RangeNodes = rangeNodes
        member this.IntermediateNodes = intermediateNodes

let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure
let Epsilon:int<terminalSymbol> = -1<terminalSymbol>
