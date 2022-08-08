module CFPQ_GLL.InputGraph

open System.Collections.Generic
open CFPQ_GLL.Common

[<Measure>] type inputGraphVertex

type InputGraphVertexBase () =
    let outgoingEdges = Dictionary<int<terminalSymbol>, HashSet<IInputGraphVertex>>()
    let descriptors = ResizeArray<Descriptor>()
    let terminalNodes = Dictionary<IInputGraphVertex, Dictionary<int<terminalSymbol>, ITerminalNode>>()
    let nonTerminalNodes = Dictionary<IInputGraphVertex, Dictionary<IRsmState, INonTerminalNode>>()
    let rangeNodes = Dictionary<MatchedRange, IRangeNode>()
    let intermediateNodes = Dictionary<MatchedRange, Dictionary<MatchedRange, IIntermediateNode>>()
    interface IInputGraphVertex with
        member this.OutgoingEdges = outgoingEdges
        member this.Descriptors = descriptors
        member this.TerminalNodes = terminalNodes
        member this.NonTerminalNodes = nonTerminalNodes    
        member this.RangeNodes = rangeNodes
        member this.IntermediateNodes = intermediateNodes
    
let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure