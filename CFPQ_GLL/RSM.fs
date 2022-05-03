module CFPQ_GLL.RSM

open CFPQ_GLL

[<Measure>] type rsmState
[<Measure>] type rsmTerminalEdge
[<Measure>] type rsmCFGEdge

type RSMEdges =
    | CFGEdge of int<rsmState>*int<rsmState>
    | TerminalEdge of int<rsmState>*int<terminalSymbol>*int<rsmState>
    | NonTerminalEdge of int<rsmState>*int<rsmState>

[<Struct>]
type RSMTerminalEdge =
    val State : int<rsmState>
    val TerminalSymbol : int<terminalSymbol>
    new (state, terminalSymbol) = {State = state; TerminalSymbol = terminalSymbol}

[<Struct>]
type RSMVertexContent =
    val OutgoingTerminalEdges : array<int64<rsmTerminalEdge>>
    val OutgoingCFGEdges : array<int64<rsmCFGEdge>>
    val OutgoingNonTerminalEdge: Option<int<rsmState>>
    new (terminalEdges, cfgEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges
            ; OutgoingCFGEdges = cfgEdges
            ; OutgoingNonTerminalEdge = nonTerminalEdges
        }
    
[<Struct>]
type RSMVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<int64<rsmTerminalEdge>>
    val OutgoingCFGEdges : ResizeArray<int64<rsmCFGEdge>>
    val OutgoingNonTerminalEdge: ResizeArray<int<rsmState>>
    new (terminalEdges, cfgEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges
            ; OutgoingCFGEdges = cfgEdges
            ; OutgoingNonTerminalEdge = nonTerminalEdges
        }
    
    
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES <<< 2 * BITS_FOR_GRAPH_VERTICES)
let MASK_FOR_INPUT_SYMBOL = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)
let RSM_VERTEX_MAX_VALUE = System.UInt32.MaxValue >>> 32 - BITS_FOR_RSM_STATE

let inline packRSMCFGEdge (targetVertex:int<rsmState>): int64<rsmCFGEdge> =
    if uint32 targetVertex > RSM_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{RSM_VERTEX_MAX_VALUE}" 
    let _targetGssVertex = (int64 targetVertex) <<< (2 * BITS_FOR_GRAPH_VERTICES)    
    (_targetGssVertex) |> LanguagePrimitives.Int64WithMeasure

let inline packRSMTerminalEdge (targetVertex:int<rsmState>) (symbol:int<terminalSymbol>) : int64<rsmTerminalEdge> =
    if uint32 targetVertex > RSM_VERTEX_MAX_VALUE
    then failwithf $"Graph vertex should be less then %A{RSM_VERTEX_MAX_VALUE}"
    if uint32 symbol > RSM_VERTEX_MAX_VALUE
    then failwithf $"Symbol should be less then %A{RSM_VERTEX_MAX_VALUE}"
    let _targetGssVertex = (int64 targetVertex) <<< (2 * BITS_FOR_GRAPH_VERTICES)
    let _symbol = int64 symbol
    (_targetGssVertex ||| _symbol) |> LanguagePrimitives.Int64WithMeasure

let inline unpackRSMCFGEdge (edge:int64<rsmCFGEdge>) : int<rsmState> =
    let edge = int64 edge
    let nextState = int32 (edge &&& MASK_FOR_RSM_STATE >>> 2 * BITS_FOR_GRAPH_VERTICES) |> LanguagePrimitives.Int32WithMeasure
    nextState
    
let inline unpackRSMTerminalEdge (edge:int64<rsmTerminalEdge>) =
    let edge = int64 edge
    let nextVertex = int32 (edge &&& MASK_FOR_RSM_STATE >>> 2 * BITS_FOR_GRAPH_VERTICES) |> LanguagePrimitives.Int32WithMeasure
    let symbol = int32 (edge &&& MASK_FOR_INPUT_SYMBOL) |> LanguagePrimitives.Int32WithMeasure
    RSMTerminalEdge(nextVertex, symbol)    

type RSM(startState:int<rsmState>, finalStates:System.Collections.Generic.HashSet<int<rsmState>>, transitions) =
    let vertices = System.Collections.Generic.Dictionary<int<rsmState>,RSMVertexContent>()
    do
        let mutableVertices = System.Collections.Generic.Dictionary<int<rsmState>,RSMVertexMutableContent>()
        let addVertex v =
            if not <| mutableVertices.ContainsKey v
            then mutableVertices.Add(v,RSMVertexMutableContent(ResizeArray<_>(),ResizeArray<_>(),ResizeArray<_>()))
            mutableVertices.[v]
        transitions
        |> Array.iter (function
                         CFGEdge (_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packRSMCFGEdge _to |> vertexContent.OutgoingCFGEdges.Add
                        | TerminalEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            packRSMTerminalEdge _to smb |> vertexContent.OutgoingTerminalEdges.Add
                        | NonTerminalEdge (_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            vertexContent.OutgoingNonTerminalEdge.Add _to)
        mutableVertices
        |> Seq.iter (fun kvp -> vertices.Add(kvp.Key, RSMVertexContent(kvp.Value.OutgoingTerminalEdges.ToArray()
                                                                              , kvp.Value.OutgoingCFGEdges.ToArray()
                                                                              , if kvp.Value.OutgoingNonTerminalEdge.Count = 0
                                                                                then None
                                                                                else Some (kvp.Value.OutgoingNonTerminalEdge.[0]))))
    member this.StartState = startState
    member this.IsStartState state = startState = state
    member this.IsFinalState state = finalStates.Contains state
    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges
    member this.OutgoingCFGEdges v =
        vertices.[v].OutgoingCFGEdges
    member this.OutgoingNonTerminalEdge v =
        vertices.[v].OutgoingNonTerminalEdge