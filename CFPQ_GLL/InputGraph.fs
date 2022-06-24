module CFPQ_GLL.InputGraph

open CFPQ_GLL

//type IInputGraphVertex = interface end

[<Struct>]
type InputGraphEdge<'vertex> =
    val TerminalSymbol: int<terminalSymbol>
    val TargetVertex: 'vertex
    new (terminal, targetVertex) = {TerminalSymbol = terminal; TargetVertex = targetVertex}

type IInputGraph<'vertex> =
    abstract GetOutgoingEdges: 'vertex -> ResizeArray<InputGraphEdge<'vertex>>

let SYMBOL_MAX_VALUE:int<terminalSymbol>=
    System.UInt32.MaxValue >>> (32 - BITS_FOR_RSM_STATE)
    |> int
    |> fun x -> x - 1
    |> LanguagePrimitives.Int32WithMeasure
    
let EOF:int<terminalSymbol> = SYMBOL_MAX_VALUE