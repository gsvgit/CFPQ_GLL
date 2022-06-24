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

let SYMBOL_MAX_VALUE:int<terminalSymbol>=
    System.UInt32.MaxValue >>> (32 - BITS_FOR_RSM_STATE)
    |> int
    |> fun x -> x - 1
    |> LanguagePrimitives.Int32WithMeasure
    
let EOF:int<terminalSymbol> = SYMBOL_MAX_VALUE