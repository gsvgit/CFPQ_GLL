module CFPQ_GLL.InputGraph

open CFPQ_GLL

[<Measure>] type graphVertex
[<Measure>] type inputGraphCallEdge
[<Measure>] type inputGraphReturnEdge
[<Measure>] type inputGraphCFGEdge

type InputGraphEdge =
    | CFGEdge of int<graphVertex>*int<graphVertex>
    | CallEdge of int<graphVertex>*int<callSymbol>*int<graphVertex>
    | ReturnEdge of int<graphVertex>*int<returnSymbol>*int<graphVertex>

[<Struct>]
type InputGraphVertexContent =
    val OutgoingCallEdges : array<int64<inputGraphCallEdge>>
    val OutgoingReturnEdges : array<int64<inputGraphReturnEdge>>
    val OutgoingCFGEdges : array<int64<inputGraphCFGEdge>>
    new (callEdges, returnEdges, cfgEdges) = {OutgoingCallEdges = callEdges; OutgoingReturnEdges = returnEdges; OutgoingCFGEdges = cfgEdges}

[<Struct>]
type InputGraphVertexMutableContent =
    val OutgoingCallEdges : ResizeArray<int64<inputGraphCallEdge>>
    val OutgoingReturnEdges : ResizeArray<int64<inputGraphReturnEdge>>
    val OutgoingCFGEdges : ResizeArray<int64<inputGraphCFGEdge>>
    new (callEdges, returnEdges, cfgEdges) = {OutgoingCallEdges = callEdges; OutgoingReturnEdges = returnEdges; OutgoingCFGEdges = cfgEdges}

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
let MASK_FOR_INPUT_SYMBOL = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)
let GRAPH_VERTEX_MAX_VALUE = System.Int32.MaxValue >>> 32 - BITS_FOR_GRAPH_VERTICES
let SYMBOL_MAX_VALUE = System.Int32.MaxValue >>> 32 - BITS_FOR_RSM_STATE

let packInputGraphCFGEdge (targetVertex:int<graphVertex>): int64<inputGraphCFGEdge> =
    if int targetVertex > GRAPH_VERTEX_MAX_VALUE
    then failwithf "Graph vertex should be less then %A" GRAPH_VERTEX_MAX_VALUE 
    let _targetGssVertex = (int64 targetVertex) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)    
    (_targetGssVertex) |> LanguagePrimitives.Int64WithMeasure

let private packInputGraphCallOrReturnEdge (targetVertex:int<graphVertex>) (symbol:int) : int64 =
    if int targetVertex > GRAPH_VERTEX_MAX_VALUE
    then failwithf "Graph vertex should be less then %A" GRAPH_VERTEX_MAX_VALUE
    if symbol > SYMBOL_MAX_VALUE
    then failwithf "Symbol should be less then %A" SYMBOL_MAX_VALUE
    let _targetGssVertex = (int64 targetVertex) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
    let _symbol = int64 symbol
    (_targetGssVertex ||| _symbol)

let packInputGraphCallEdge (targetVertex:int<graphVertex>) (symbol:int<callSymbol>) : int64<inputGraphCallEdge> =
    packInputGraphCallOrReturnEdge targetVertex (int symbol)|> LanguagePrimitives.Int64WithMeasure

let packInputGraphReturnEdge (targetVertex:int<graphVertex>) (symbol:int<returnSymbol>) : int64<inputGraphReturnEdge> =
    packInputGraphCallOrReturnEdge targetVertex (int symbol)|> LanguagePrimitives.Int64WithMeasure
   
let private unpackInputGraphCallOrReturnEdge (edge:int64) : int<graphVertex> * int =    
    let nextVertex = int32 (edge &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let symbol = int32 (edge &&& MASK_FOR_INPUT_SYMBOL) 
    nextVertex, symbol
let unpackInputGraphCFGEdge (edge:int64<inputGraphCFGEdge>) : int<graphVertex> =
    let edge = int64 edge
    let nextVertex = int32 (edge &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    nextVertex
    
let unpackInputGraphCallEdge (edge:int64<inputGraphCallEdge>) : int<graphVertex> * int<callSymbol> =
    let v,s = unpackInputGraphCallOrReturnEdge (int64 edge)
    v, s |> LanguagePrimitives.Int32WithMeasure

let unpackInputGraphReturnEdge (edge:int64<inputGraphReturnEdge>) : int<graphVertex> * int<returnSymbol> =
    let v,s = unpackInputGraphCallOrReturnEdge (int64 edge)
    v, s |> LanguagePrimitives.Int32WithMeasure

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
                        | CallEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packInputGraphCallEdge _to smb |> vertexContent.OutgoingCallEdges.Add
                        | ReturnEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packInputGraphReturnEdge _to smb |> vertexContent.OutgoingReturnEdges.Add )
        mutableVertices
        |> Seq.iter (fun kvp -> vertices.Add(kvp.Key, InputGraphVertexContent(kvp.Value.OutgoingCallEdges.ToArray()
                                                                              , kvp.Value.OutgoingReturnEdges.ToArray()
                                                                              , kvp.Value.OutgoingCFGEdges.ToArray())))
    member this.OutgoingCallEdges v =
        vertices.[v].OutgoingCallEdges
    member this.OutgoingReturnEdges v =
        vertices.[v].OutgoingReturnEdges
    member this.OutgoingCFGEdges v =
        vertices.[v].OutgoingCFGEdges    
    