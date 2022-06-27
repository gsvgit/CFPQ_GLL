module CFPQ_GLL.InputGraph


[<Measure>] type inputGraphVertex
[<Measure>] type terminalSymbol

[<Struct>]
type InputGraphEdge<'inputGraphVertex> =
    val TerminalSymbol: int<terminalSymbol>
    val TargetVertex: 'inputGraphVertex
    new (terminal, targetVertex) = {TerminalSymbol = terminal; TargetVertex = targetVertex}

type IInputGraph<'inputGraphVertex> =
    abstract GetOutgoingEdges: 'inputGraphVertex -> ResizeArray<InputGraphEdge<'inputGraphVertex>>
    
let EOF:int<terminalSymbol> = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure