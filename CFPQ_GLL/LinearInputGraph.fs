module CFPQ_GLL.InputGraph

open System.Collections.Generic
open CFPQ_GLL.Common

[<Measure>] type inputGraphVertex

type LinearInputGraphVertexBase (id:int32) =
    let id = id
    let mutable outgoingEdge = None
    let descriptors = HashSet<Descriptor>()
    let terminalNodes = Dictionary<ILinearInputGraphVertex, Dictionary<int<terminalSymbol>, ITerminalNode>>()
    let nonTerminalNodes = Dictionary<ILinearInputGraphVertex, Dictionary<IRsmState, INonTerminalNode>>()
    let rangeNodes = Dictionary<MatchedRange, IRangeNode>()
        //Dictionary<MatchedRange, IRangeNode>()
    let intermediateNodes = Dictionary<MatchedRange, Dictionary<MatchedRange, IIntermediateNode>>()
    override this.GetHashCode() = id
    
    member this.AddOutgoingEdge (terminal, target) =
        match outgoingEdge with
        | None -> outgoingEdge <- Some (terminal, target)
        | Some x -> failwithf $"Edge exists: %A{x}"

    interface ILinearInputGraphVertex with
        member this.Id = id
        member this.OutgoingEdge =
            match outgoingEdge with
            | Some v -> v
            | None -> failwith "Unexpected end of input"
        member this.Descriptors = descriptors
        member this.TerminalNodes = terminalNodes
        member this.NonTerminalNodesStartedHere = nonTerminalNodes
        member this.RangeNodes = rangeNodes
        member this.IntermediateNodes = intermediateNodes

let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure
let Epsilon:int<terminalSymbol> = -1<terminalSymbol>
