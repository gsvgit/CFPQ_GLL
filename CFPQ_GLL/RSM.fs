module CFPQ_GLL.RSM

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open FSharpx.Collections

[<Measure>] type rsmState
[<Measure>] type rsmTerminalEdge
[<Measure>] type rsmNonTerminalEdge

type RSMEdges =    
    | TerminalEdge of int<rsmState>*int<terminalSymbol>*int<rsmState>
    | NonTerminalEdge of _from:int<rsmState>*_nonTerminalSymbolStartState:int<rsmState>*_to:int<rsmState>

[<Struct>]
type RSMTerminalEdge =
    val State : int<rsmState>
    val TerminalSymbol : int<terminalSymbol>
    new (state, terminalSymbol) = {State = state; TerminalSymbol = terminalSymbol}

[<Struct>]
type RSMNonTerminalEdge =
    val State : int<rsmState>
    val NonTerminalSymbolStartState : int<rsmState>
    new (state, nonTerminalSymbolStartState) = {State = state; NonTerminalSymbolStartState = nonTerminalSymbolStartState}

type TerminalEdgesStorage =
    | Small of array<int64<rsmTerminalEdge>>
    | Big of SortedDictionary<int<terminalSymbol>,int<rsmState>>
    
[<Struct>]
type RSMVertexContent =
    val OutgoingTerminalEdges : TerminalEdgesStorage
    val OutgoingNonTerminalEdges: array<int64<rsmNonTerminalEdge>>
    new (terminalEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges            
            OutgoingNonTerminalEdges = nonTerminalEdges
        }
    
[<Struct>]
type RSMVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<int64<rsmTerminalEdge>>    
    val OutgoingNonTerminalEdges: ResizeArray<int64<rsmNonTerminalEdge>>
    new (terminalEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges            
            OutgoingNonTerminalEdges = nonTerminalEdges
        }

let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> (2 * BITS_FOR_GRAPH_VERTICES) <<< (2 * BITS_FOR_GRAPH_VERTICES))
let MASK_FOR_INPUT_SYMBOL = int64 (System.UInt64.MaxValue >>> (2 * BITS_FOR_GRAPH_VERTICES))
let RSM_VERTEX_MAX_VALUE:int<rsmState> =
    System.UInt32.MaxValue >>> (32 - BITS_FOR_RSM_STATE)
    |> int
    |> fun x -> x - 1
    |> LanguagePrimitives.Int32WithMeasure

let inline private packRSMTerminalOrNonTerminalEdge (targetVertex:int<rsmState>) (symbol:int) : int64 =
    if targetVertex > RSM_VERTEX_MAX_VALUE
    then failwithf $"RSM vertex should be less then %A{RSM_VERTEX_MAX_VALUE}"
    if symbol > int32 RSM_VERTEX_MAX_VALUE
    then failwithf $"Symbol should be less then %A{RSM_VERTEX_MAX_VALUE}"
    
    let _targetVertex = (int64 targetVertex) <<< BITS_FOR_RSM_STATE
    let _symbol = int64 symbol
    (_targetVertex ||| _symbol)


let inline packRSMTerminalEdge (targetVertex:int<rsmState>) (symbol:int<terminalSymbol>) : int64<rsmTerminalEdge> =
    packRSMTerminalOrNonTerminalEdge targetVertex (int symbol) |> LanguagePrimitives.Int64WithMeasure

let inline packRSMNonTerminalEdge (targetVertex:int<rsmState>) (nonTerminalSymbolStartState:int<rsmState>) : int64<rsmNonTerminalEdge> =
    packRSMTerminalOrNonTerminalEdge targetVertex (int nonTerminalSymbolStartState) |> LanguagePrimitives.Int64WithMeasure
    
let inline unpackRSMTerminalOrNonTerminalEdge (edge:int64) =
    let nextVertex = int32 (edge >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let symbol = int32 (edge &&& MASK_FOR_INPUT_SYMBOL) |> LanguagePrimitives.Int32WithMeasure   
    nextVertex, symbol

let inline unpackRSMTerminalEdge (edge:int64<rsmTerminalEdge>) =
    let nextVertex, symbol = unpackRSMTerminalOrNonTerminalEdge (int64 edge)
    RSMTerminalEdge(nextVertex, symbol)
    
let inline unpackRSMNonTerminalEdge (edge:int64<rsmNonTerminalEdge>) =
    let nextVertex, symbol = unpackRSMTerminalOrNonTerminalEdge (int64 edge)
    RSMNonTerminalEdge(nextVertex, symbol)    

type RSMBox(startState:int<rsmState>, finalStates:HashSet<int<rsmState>>, transitions) =
    member this.StartState = startState
    member this.FinalStates = finalStates
    member this.Transitions = transitions
    
type RSM(boxes:array<RSMBox>, startBox:RSMBox) =
    let vertices = Dictionary<int<rsmState>,RSMVertexContent>()
    let finalStates = HashSet<_>()
    let finalStatesForBox = Dictionary<int<rsmState>,ResizeArray<_>>()
    let startStateOfExtendedRSM = RSM_VERTEX_MAX_VALUE
        
    let extensionBox =
        let originalStartState = startBox.StartState        
        let finalState = int32 startStateOfExtendedRSM - 2 |> LanguagePrimitives.Int32WithMeasure
        let intermediateState = int32 startStateOfExtendedRSM - 1 |> LanguagePrimitives.Int32WithMeasure
        let startState = startStateOfExtendedRSM
        RSMBox(startState
               , HashSet<_>([|finalState|])
               , [|
                    NonTerminalEdge(startState, originalStartState, intermediateState)
                    RSMEdges.TerminalEdge(intermediateState, EOF, finalState)
                 |])

    let addBoxes (rsmBoxes: array<RSMBox>) =
        let mutableVertices = Dictionary<int<rsmState>,RSMVertexMutableContent>()
        let addVertex v =
            if not <| mutableVertices.ContainsKey v
            then mutableVertices.Add(v,RSMVertexMutableContent(ResizeArray<_>(),ResizeArray<_>()))
            mutableVertices.[v]
        rsmBoxes        
        |> Array.iter (fun box ->
            addVertex box.StartState |> ignore
            box.FinalStates |> Seq.iter (finalStates.Add >> ignore)
            box.FinalStates |> Seq.iter (addVertex>>ignore)
            finalStatesForBox.Add(box.StartState, box.FinalStates |> ResizeArray.ofSeq)
            box.Transitions
            |> Array.iter(
                    function
                    | TerminalEdge (_from, smb, _to) ->
                        let vertexContent = addVertex _from
                        addVertex _to |> ignore
                        packRSMTerminalEdge _to smb |> vertexContent.OutgoingTerminalEdges.Add
                    | NonTerminalEdge (_from, _nonTerminalStartState, _to) ->
                        let vertexContent = addVertex _from
                        addVertex _to |> ignore
                        packRSMNonTerminalEdge _to _nonTerminalStartState |> vertexContent.OutgoingNonTerminalEdges.Add
                        )
            )
        mutableVertices
        |> Seq.iter (fun kvp ->
              let edges = kvp.Value.OutgoingTerminalEdges.ToArray()
              let storedEdges =
                  if edges.Length <= 50
                  then Small edges
                  else
                      let dict = SortedDictionary<_,_>()
                      for e in edges do
                          let edge = unpackRSMTerminalEdge e
                          dict.Add(edge.TerminalSymbol, edge.State)
                      Big dict
                          
              vertices.Add(kvp.Key, RSMVertexContent(storedEdges                                                                              
              , kvp.Value.OutgoingNonTerminalEdges.ToArray())))

    do
        addBoxes boxes
        addBoxes [|extensionBox|]
    member this.StartState = startStateOfExtendedRSM
    member this.IsFinalState state = finalStates.Contains state
    member this.IsFinalStateForOriginalStartBox state = startBox.FinalStates.Contains state
    member this.GetFinalStatesForBoxWithThisStartState startState = finalStatesForBox.[startState]
    member this.OriginalStartState = startBox.StartState
    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges    
    member this.OutgoingNonTerminalEdges v =
        vertices.[v].OutgoingNonTerminalEdges