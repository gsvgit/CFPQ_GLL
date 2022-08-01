module CFPQ_GLL.InputGraph

open System.Collections.Generic
open CFPQ_GLL.Common


[<Measure>] type inputGraphVertex


[<Struct>]
type InputGraphEdge =
    val TerminalSymbol: int<terminalSymbol>
    val TargetVertex: int<inputGraphVertex>
    new (terminal, targetVertex) = {TerminalSymbol = terminal; TargetVertex = targetVertex}

    
type IInputGraph =
    abstract GetOutgoingEdges: int<inputGraphVertex> -> ResizeArray<InputGraphEdge>
    
let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure