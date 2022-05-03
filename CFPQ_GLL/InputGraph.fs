module CFPQ_GLL.InputGraph

open CFPQ_GLL

[<Measure>] type graphVertex
[<Measure>] type inputGraphTerminalEdge
[<Measure>] type inputGraphCFGEdge

type InputGraphEdge =
    | CFGEdge of int<graphVertex>*int<graphVertex>
    | TerminalEdge of int<graphVertex>*int<terminalSymbol>*int<graphVertex>
    
[<Struct>]
type InputGraphTerminalEdge =
    val Vertex : int<graphVertex>
    val TerminalSymbol : int<terminalSymbol>
    new (vertex, terminalSymbol) = {Vertex = vertex; TerminalSymbol = terminalSymbol}

[<Struct>]
type InputGraphVertexContent =
    val OutgoingTerminalEdges : array<int64<inputGraphTerminalEdge>>
    val OutgoingCFGEdges : array<int64<inputGraphCFGEdge>>
    new (terminalEdges, cfgEdges) = {OutgoingTerminalEdges = terminalEdges; OutgoingCFGEdges = cfgEdges}

[<Struct>]
type InputGraphVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<int64<inputGraphTerminalEdge>>
    val OutgoingCFGEdges : ResizeArray<int64<inputGraphCFGEdge>>
    new (terminalEdges, returnEdges, cfgEdges) = {OutgoingTerminalEdges = terminalEdges; OutgoingCFGEdges = cfgEdges}

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
let MASK_FOR_INPUT_SYMBOL = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)
let GRAPH_VERTEX_MAX_VALUE = System.UInt32.MaxValue >>> 32 - BITS_FOR_GRAPH_VERTICES
let SYMBOL_MAX_VALUE = System.UInt32.MaxValue >>> 32 - BITS_FOR_RSM_STATE

let inline packInputGraphCFGEdge (targetVertex:int<graphVertex>): int64<inputGraphCFGEdge> =
    if uint32 targetVertex > GRAPH_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{GRAPH_VERTEX_MAX_VALUE}" 
    let _targetGssVertex = (int64 targetVertex) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)    
    (_targetGssVertex) |> LanguagePrimitives.Int64WithMeasure

let inline private packInputGraphTerminalEdge (targetVertex:int<graphVertex>) (symbol:int<terminalSymbol>) : int64<inputGraphTerminalEdge> =
    if uint32 targetVertex > GRAPH_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{GRAPH_VERTEX_MAX_VALUE}"
    if uint32 symbol > SYMBOL_MAX_VALUE
    then failwithf $"Symbol should be less then %A{SYMBOL_MAX_VALUE}"
    let _targetGssVertex = (int64 targetVertex) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
    let _symbol = int64 symbol
    (_targetGssVertex ||| _symbol) |> LanguagePrimitives.Int64WithMeasure
   
let inline unpackInputGraphTerminalEdge (edge:int64<inputGraphTerminalEdge>) =
    let edge = int64 edge
    let nextVertex = int32 (edge &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let symbol = int32 (edge &&& MASK_FOR_INPUT_SYMBOL) |> LanguagePrimitives.Int32WithMeasure
    InputGraphTerminalEdge(nextVertex, symbol)
let inline unpackInputGraphCFGEdge (edge:int64<inputGraphCFGEdge>) : int<graphVertex> =
    let edge = int64 edge
    let nextVertex = int32 (edge &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    nextVertex
    
type InputGraph (edges) =
    let vertices = System.Collections.Generic.Dictionary<int<graphVertex>,InputGraphVertexContent>()
  
    do
        let mutableVertices = System.Collections.Generic.Dictionary<int<graphVertex>,InputGraphVertexMutableContent>()
        let addVertex v =
            if not <| mutableVertices.ContainsKey v
            then mutableVertices.Add(v,InputGraphVertexMutableContent(ResizeArray<_>(),ResizeArray<_>(),ResizeArray<_>()))
            mutableVertices.[v]
        edges
        |> Array.iter (function
                         CFGEdge (_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packInputGraphCFGEdge _to |> vertexContent.OutgoingCFGEdges.Add
                        | TerminalEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packInputGraphTerminalEdge _to smb |> vertexContent.OutgoingTerminalEdges.Add
                       )
        mutableVertices
        |> Seq.iter (fun kvp -> vertices.Add(kvp.Key, InputGraphVertexContent(kvp.Value.OutgoingTerminalEdges.ToArray()
                                                                              , kvp.Value.OutgoingCFGEdges.ToArray())))
    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges
    member this.OutgoingCFGEdges v =
        vertices.[v].OutgoingCFGEdges
    member this.NumberOfVertices () = vertices.Count
    member this.AllVertices() = vertices.Keys |> Array.ofSeq
    