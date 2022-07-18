module CFPQ_GLL.RSM

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open FSharpx.Collections

[<Measure>] type rsmState

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
    | Small of array<RSMTerminalEdge>
    | Big of SortedDictionary<int<terminalSymbol>,int<rsmState>>
    
[<Struct>]
type RSMVertexContent =
    val OutgoingTerminalEdges : TerminalEdgesStorage
    val OutgoingNonTerminalEdges: array<RSMNonTerminalEdge>
    new (terminalEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges            
            OutgoingNonTerminalEdges = nonTerminalEdges
        }
    
[<Struct>]
type RSMVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<RSMTerminalEdge>    
    val OutgoingNonTerminalEdges: ResizeArray<RSMNonTerminalEdge>
    new (terminalEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges            
            OutgoingNonTerminalEdges = nonTerminalEdges
        }

type RSMBox(startState:int<rsmState>, finalStates:HashSet<int<rsmState>>, transitions) =
    member this.StartState = startState
    member this.FinalStates = finalStates
    member this.Transitions = transitions
    
type RSM(boxes:array<RSMBox>, startBox:RSMBox) =
    let vertices = Dictionary<int<rsmState>,RSMVertexContent>()
    let finalStates = HashSet<_>()
    let finalStatesForBox = Dictionary<int<rsmState>,ResizeArray<_>>()
    let startStateOfExtendedRSM = System.Int32.MaxValue - 1 |> LanguagePrimitives.Int32WithMeasure
        
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
                        RSMTerminalEdge(_to, smb) |> vertexContent.OutgoingTerminalEdges.Add
                    | NonTerminalEdge (_from, _nonTerminalStartState, _to) ->
                        let vertexContent = addVertex _from
                        addVertex _to |> ignore
                        RSMNonTerminalEdge(_to, _nonTerminalStartState) |> vertexContent.OutgoingNonTerminalEdges.Add
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
                      for edge in edges do
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
    member this.OriginalFinalStates = startBox.FinalStates
    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges    
    member this.OutgoingNonTerminalEdges v =
        vertices.[v].OutgoingNonTerminalEdges
        
    member this.ToDot filePath =
        seq {
         yield "digraph g {"
         for kvp in vertices do                              
             for nonTerminalEdge in kvp.Value.OutgoingNonTerminalEdges do
                yield $"%i{kvp.Key} -> %i{nonTerminalEdge.State} [label = N_%i{nonTerminalEdge.NonTerminalSymbolStartState}]"             
             match kvp.Value.OutgoingTerminalEdges with
             | Small a ->
                 for t in a do
                     yield $"%i{kvp.Key} -> %i{t.State} [label = t_%i{t.TerminalSymbol}]"
             | Big a ->
                 for _kvp in a do
                     yield $"%i{kvp.Key} -> %i{_kvp.Value} [label = t_%i{_kvp.Key}]"                               
         yield "}"
        }
        |> fun x -> System.IO.File.WriteAllLines(filePath, x)