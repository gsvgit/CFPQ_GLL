module CFPQ_GLL.InputGraph

open System.Net.NetworkInformation
open CFPQ_GLL

[<Measure>] type graphVertex
[<Measure>] type inputGraphTerminalEdge
[<Measure>] type inputGraphCFGEdge

type InputGraphEdge =    
    | TerminalEdge of int<graphVertex>*int<terminalSymbol>*int<graphVertex>
    
[<Struct>]
type InputGraphTerminalEdge =
    val Vertex : int<graphVertex>
    val TerminalSymbol : int<terminalSymbol>
    new (vertex, terminalSymbol) = {Vertex = vertex; TerminalSymbol = terminalSymbol}

[<Struct>]
type InputGraphVertexContent =
    val OutgoingTerminalEdges : array<int64<inputGraphTerminalEdge>>    
    new (terminalEdges) = {OutgoingTerminalEdges = terminalEdges}

[<Struct>]
type InputGraphVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<int64<inputGraphTerminalEdge>>    
    new (terminalEdges) = {OutgoingTerminalEdges = terminalEdges}

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE))
let MASK_FOR_INPUT_SYMBOL = int64 (System.UInt64.MaxValue >>> (2 * BITS_FOR_GRAPH_VERTICES))
let GRAPH_VERTEX_MAX_VALUE:int<graphVertex> =
    System.UInt32.MaxValue >>> (32 - BITS_FOR_GRAPH_VERTICES)
    |> int
    |> (fun x -> x - 1)
    |> LanguagePrimitives.Int32WithMeasure
let SYMBOL_MAX_VALUE:int<terminalSymbol>=
    System.UInt32.MaxValue >>> (32 - BITS_FOR_RSM_STATE)
    |> int
    |> fun x -> x - 1
    |> LanguagePrimitives.Int32WithMeasure
let EOF:int<terminalSymbol> = SYMBOL_MAX_VALUE

let inline private packInputGraphTerminalEdge (targetVertex:int<graphVertex>) (symbol:int<terminalSymbol>) : int64<inputGraphTerminalEdge> =
    if targetVertex > GRAPH_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{GRAPH_VERTEX_MAX_VALUE}"
    if symbol > SYMBOL_MAX_VALUE
    then failwithf $"Symbol should be less then %A{SYMBOL_MAX_VALUE}"
    let _targetGssVertex = (int64 targetVertex) <<< BITS_FOR_RSM_STATE
    let _symbol = int64 symbol
    (_targetGssVertex ||| _symbol) |> LanguagePrimitives.Int64WithMeasure
   
let inline unpackInputGraphTerminalEdge (edge:int64<inputGraphTerminalEdge>) =
    let edge = int64 edge
    let nextVertex = int32 (edge >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let symbol = int32 (edge &&& MASK_FOR_INPUT_SYMBOL) |> LanguagePrimitives.Int32WithMeasure
    InputGraphTerminalEdge(nextVertex, symbol)
    
type InputGraph (edges) =
    let vertices = System.Collections.Generic.Dictionary<int<graphVertex>,InputGraphVertexMutableContent>()
  
    let sinkVertex = int32 MASK_FOR_INPUT_POSITION |> LanguagePrimitives.Int32WithMeasure 
    
    let addEdges edges =
        let addVertex v =
            if not <| vertices.ContainsKey v
            then
                vertices.Add(v,InputGraphVertexMutableContent(ResizeArray<_>()))
                if v <> sinkVertex
                then vertices.[v].OutgoingTerminalEdges.Add(packInputGraphTerminalEdge sinkVertex EOF)
            vertices.[v]
        edges
        |> Array.iter (function                         
                        | TerminalEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packInputGraphTerminalEdge _to smb |> vertexContent.OutgoingTerminalEdges.Add
                       )
        addVertex sinkVertex |> ignore
    
    do addEdges edges

    new () = InputGraph([||])    
    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges    
    member this.NumberOfVertices() = vertices.Count
    member this.AllVertices() = vertices.Keys |> Array.ofSeq
    member this.ToDot(drawSink, file) =
        seq{
            yield "digraph InputGraph{"
            yield "node [shape = plaintext]"
            
            for kvp in vertices do
                for edge in kvp.Value.OutgoingTerminalEdges do
                    let edge = unpackInputGraphTerminalEdge edge
                    if edge.Vertex <> sinkVertex || drawSink
                    then yield $"%i{kvp.Key} -> %i{edge.Vertex} [label=%i{edge.TerminalSymbol}]"
      
            yield "}"
        }
        |> fun data -> System.IO.File.WriteAllLines(file, data)
            
    member this.RemoveOutgoingEdges v = vertices.[v].OutgoingTerminalEdges.Clear()    
    member this.AddEdges edges =
        addEdges edges
        
    