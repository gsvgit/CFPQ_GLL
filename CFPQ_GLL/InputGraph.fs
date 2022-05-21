module CFPQ_GLL.InputGraph

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
    |> int32
    |> LanguagePrimitives.Int32WithMeasure
let SYMBOL_MAX_VALUE = System.UInt32.MaxValue >>> (32 - BITS_FOR_RSM_STATE)
let EOF:int<terminalSymbol> = 1000 |> LanguagePrimitives.Int32WithMeasure // int32 SYMBOL_MAX_VALUE |> LanguagePrimitives.Int32WithMeasure

let inline private packInputGraphTerminalEdge (targetVertex:int<graphVertex>) (symbol:int<terminalSymbol>) : int64<inputGraphTerminalEdge> =
    if targetVertex > GRAPH_VERTEX_MAX_VALUE
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
    
type InputGraph (edges) =
    let vertices = System.Collections.Generic.Dictionary<int<graphVertex>,InputGraphVertexContent>()
  
    let sinkVertex = int32 MASK_FOR_INPUT_POSITION |> LanguagePrimitives.Int32WithMeasure 
    
    do
        let mutableVertices = System.Collections.Generic.Dictionary<int<graphVertex>,InputGraphVertexMutableContent>()
        let addVertex v =
            if not <| mutableVertices.ContainsKey v
            then mutableVertices.Add(v,InputGraphVertexMutableContent(ResizeArray<_>()))
            mutableVertices.[v]
        edges
        |> Array.iter (function                         
                        | TerminalEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packInputGraphTerminalEdge _to smb |> vertexContent.OutgoingTerminalEdges.Add
                       )
        addVertex sinkVertex |> ignore
        mutableVertices
        |> Seq.iter (fun kvp ->
            kvp.Value.OutgoingTerminalEdges.Add (packInputGraphTerminalEdge sinkVertex EOF)
            vertices.Add(kvp.Key, InputGraphVertexContent(kvp.Value.OutgoingTerminalEdges.ToArray())))
    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges    
    member this.NumberOfVertices () = vertices.Count
    member this.AllVertices() = vertices.Keys |> Array.ofSeq
    