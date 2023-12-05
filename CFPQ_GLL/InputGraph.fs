module CFPQ_GLL.InputGraph

open System
open System.Collections.Generic
open CFPQ_GLL.Common

[<Measure>] type inputGraphVertex

type InputGraphVertexBase () =
    let outgoingEdges = Dictionary<int<terminalSymbol>, HashSet<IInputGraphVertex>>()
    let descriptors = HashSet<WeakReference<Descriptor>>()
    let terminalNodes = Dictionary<IInputGraphVertex, Dictionary<int<terminalSymbol>, WeakReference<ITerminalNode>>>()
    let nonTerminalNodes = Dictionary<IInputGraphVertex, Dictionary<IRsmState, WeakReference<INonTerminalNode>>>()
    let rangeNodes = Dictionary<MatchedRange, WeakReference<IRangeNode>>()
    let intermediateNodes = Dictionary<MatchedRange, Dictionary<MatchedRange, WeakReference<IIntermediateNode>>>()
    //let nonTerminalsWithStartHere = HashSet<_>()
    interface IInputGraphVertex with
        member this.OutgoingEdges = outgoingEdges
        member this.Descriptors = descriptors
        member this.GetValidDescriptors() =
            let count = descriptors.RemoveWhere(fun d -> let isAlive, d = d.TryGetTarget() in isAlive && d.IsAlive)
            descriptors |> Seq.map (fun d -> let _,d = d.TryGetTarget() in d)
        member this.TerminalNodes = terminalNodes
        member this.NonTerminalNodesStartedHere = nonTerminalNodes
        //member this.NonTerminalNodesWithStartHere = nonTerminalsWithStartHere
        member this.RangeNodes = rangeNodes
        member this.IntermediateNodes = intermediateNodes

let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure
let Epsilon:int<terminalSymbol> = -1<terminalSymbol>
