module CFPQ_GLL.RSM

[<Measure>] type rsmState
[<Measure>] type rsmCallEdge
[<Measure>] type rsmReturnEdge
[<Measure>] type rsmCFGEdge

type RSMEdges =
    | CFGEdge of int<rsmState>*int<rsmState>
    | CallEdge of int<rsmState>*int<callSymbol>*int<rsmState>
    | ReturnEdge of int<rsmState>*int<returnSymbol>*int<rsmState>
    | NonTerminalEdge of int<rsmState>*int<rsmState>

[<Struct>]
type RSMCallEdge =
    val State : int<rsmState>
    val CallSymbol : int<callSymbol>
    new (state, callSymbol) = {State = state; CallSymbol = callSymbol}

[<Struct>]
type RSMReturnEdge =
    val State : int<rsmState>
    val ReturnSymbol : int<returnSymbol>
    new (state, returnSymbol) = {State = state; ReturnSymbol = returnSymbol}

[<Struct>]
type RSMCallOrReturnEdge =
    val State : int<rsmState>
    val Symbol : int
    new (state, symbol) = {State = state; Symbol = symbol}


[<Struct>]
type RSMVertexContent =
    val OutgoingCallEdges : array<int64<rsmCallEdge>>
    val OutgoingReturnEdges : array<int64<rsmReturnEdge>>
    val OutgoingCFGEdges : array<int64<rsmCFGEdge>>
    val OutgoingNonTerminalEdge: Option<int<rsmState>>
    new (_call, _return, _cfg, _nonTerm) = {OutgoingCallEdges = _call
                                            ; OutgoingReturnEdges = _return
                                            ; OutgoingCFGEdges = _cfg
                                            ; OutgoingNonTerminalEdge = _nonTerm}
    
[<Struct>]
type RSMVertexMutableContent =
    val OutgoingCallEdges : ResizeArray<int64<rsmCallEdge>>
    val OutgoingReturnEdges : ResizeArray<int64<rsmReturnEdge>>
    val OutgoingCFGEdges : ResizeArray<int64<rsmCFGEdge>>
    val OutgoingNonTerminalEdge: ResizeArray<int<rsmState>>
    new (_call,_return,_cfg,_nonTerm) = {OutgoingCallEdges = _call
                                            ; OutgoingReturnEdges = _return
                                            ; OutgoingCFGEdges = _cfg
                                            ; OutgoingNonTerminalEdge = _nonTerm}
    
    
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES <<< 2 * BITS_FOR_GRAPH_VERTICES)
let MASK_FOR_INPUT_SYMBOL = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)
let RSM_VERTEX_MAX_VALUE = System.UInt32.MaxValue >>> 32 - BITS_FOR_RSM_STATE

let packRSMCFGEdge (targetVertex:int<rsmState>): int64<rsmCFGEdge> =
    if uint32 targetVertex > RSM_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{RSM_VERTEX_MAX_VALUE}" 
    let _targetGssVertex = (int64 targetVertex) <<< (2 * BITS_FOR_GRAPH_VERTICES)    
    (_targetGssVertex) |> LanguagePrimitives.Int64WithMeasure

let private packRSMCallOrReturnOrNonTerminalEdge (targetVertex:int<rsmState>) (symbol:int) : int64 =
    if uint32 targetVertex > RSM_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{RSM_VERTEX_MAX_VALUE}"
    if uint32 symbol > RSM_VERTEX_MAX_VALUE
    then failwithf $"Symbol should be less then %A{RSM_VERTEX_MAX_VALUE}"
    let _targetGssVertex = (int64 targetVertex) <<< (2 * BITS_FOR_GRAPH_VERTICES)
    let _symbol = int64 symbol
    (_targetGssVertex ||| _symbol)

let packRSMCallEdge (targetVertex:int<rsmState>) (symbol:int<callSymbol>) : int64<rsmCallEdge> =
    packRSMCallOrReturnOrNonTerminalEdge targetVertex (int symbol)|> LanguagePrimitives.Int64WithMeasure

let packRSMReturnEdge (targetVertex:int<rsmState>) (symbol:int<returnSymbol>) : int64<rsmReturnEdge> =
    packRSMCallOrReturnOrNonTerminalEdge targetVertex (int symbol)|> LanguagePrimitives.Int64WithMeasure


let unpackRSMCFGEdge (edge:int64<rsmCFGEdge>) : int<rsmState> =
    let edge = int64 edge
    let nextState = int32 (edge &&& MASK_FOR_RSM_STATE >>> 2 * BITS_FOR_GRAPH_VERTICES) |> LanguagePrimitives.Int32WithMeasure
    nextState
    
let private unpackRSMCallOrReturnEdge (edge:int64) =    
    let nextVertex = int32 (edge &&& MASK_FOR_RSM_STATE >>> 2 * BITS_FOR_GRAPH_VERTICES) |> LanguagePrimitives.Int32WithMeasure
    let symbol = int32 (edge &&& MASK_FOR_INPUT_SYMBOL) 
    RSMCallOrReturnEdge(nextVertex, symbol)    

let unpackRSMCallEdge (edge:int64<rsmCallEdge>) =
    let untypedEdge = unpackRSMCallOrReturnEdge (int64 edge)
    RSMCallEdge (untypedEdge.State, untypedEdge.Symbol |> LanguagePrimitives.Int32WithMeasure)

let unpackRSMReturnEdge (edge:int64<rsmReturnEdge>) =
    let untypedEdge = unpackRSMCallOrReturnEdge (int64 edge)
    RSMReturnEdge (untypedEdge.State, untypedEdge.Symbol |> LanguagePrimitives.Int32WithMeasure)

type RSM(startState:int<rsmState>, finalStates:System.Collections.Generic.HashSet<int<rsmState>>, transitions) =
    let vertices = System.Collections.Generic.Dictionary<int<rsmState>,RSMVertexContent>()
    do
        let mutableVertices = System.Collections.Generic.Dictionary<int<rsmState>,RSMVertexMutableContent>()
        let addVertex v =
            if not <| mutableVertices.ContainsKey v
            then mutableVertices.Add(v,RSMVertexMutableContent(ResizeArray<_>(),ResizeArray<_>(),ResizeArray<_>(),ResizeArray<_>()))
            mutableVertices.[v]
        transitions
        |> Array.iter (function
                         CFGEdge (_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packRSMCFGEdge _to |> vertexContent.OutgoingCFGEdges.Add
                        | CallEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packRSMCallEdge _to smb |> vertexContent.OutgoingCallEdges.Add
                        | ReturnEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packRSMReturnEdge _to smb |> vertexContent.OutgoingReturnEdges.Add
                        | NonTerminalEdge (_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            vertexContent.OutgoingNonTerminalEdge.Add _to)
        mutableVertices
        |> Seq.iter (fun kvp -> vertices.Add(kvp.Key, RSMVertexContent(kvp.Value.OutgoingCallEdges.ToArray()
                                                                              , kvp.Value.OutgoingReturnEdges.ToArray()
                                                                              , kvp.Value.OutgoingCFGEdges.ToArray()
                                                                              , if kvp.Value.OutgoingNonTerminalEdge.Count = 0
                                                                                then None
                                                                                else Some (kvp.Value.OutgoingNonTerminalEdge.[0]))))
    member this.StartState = startState
    member this.IsStartState state = startState = state
    member this.IsFinalState state = finalStates.Contains state
    member this.OutgoingCallEdges v =
        vertices.[v].OutgoingCallEdges
    member this.OutgoingReturnEdges v =
        vertices.[v].OutgoingReturnEdges
    member this.OutgoingCFGEdges v =
        vertices.[v].OutgoingCFGEdges
    member this.OutgoingNonTerminalEdge v =
        vertices.[v].OutgoingNonTerminalEdge