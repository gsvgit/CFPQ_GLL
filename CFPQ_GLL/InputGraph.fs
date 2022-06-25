module CFPQ_GLL.InputGraph

open CFPQ_GLL

type [<Measure>] inputGraphVertex

[<Struct>]
type InputGraphEdge =
    val TerminalSymbol: int<terminalSymbol>
    val TargetVertex: int<inputGraphVertex>
    new (terminal, targetVertex) = {TerminalSymbol = terminal; TargetVertex = targetVertex}

type IInputGraph =
    abstract GetOutgoingEdges: int<inputGraphVertex> -> ResizeArray<InputGraphEdge>
    
let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure