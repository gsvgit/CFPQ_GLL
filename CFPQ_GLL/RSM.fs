module CFPQ_GLL.RSM

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open FSharpx.Collections

[<Measure>] type rsmState

type RSMEdges =    
    | TerminalEdge of int<rsmState>*int<terminalSymbol>*int<rsmState>
    | NonTerminalEdge of _from:int<rsmState>*_nonTerminalSymbolStartState:int<rsmState>*_to:int<rsmState>
    
    member this.StartState =
        match this with
        | TerminalEdge (_from,_,_)
        | NonTerminalEdge (_from,_,_) -> _from
        
    member this.FinalState =
        match this with
        | TerminalEdge (_,_,_to)
        | NonTerminalEdge (_,_,_to) -> _to
    
    member this.Terminal =
        match this with
        | TerminalEdge (_,t,_) -> t
        | NonTerminalEdge _ -> failwith "Cannot get terminal from nonterminal edge."
        
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
    | Big of Dictionary<int<terminalSymbol>,ResizeArray<int<rsmState>>>
    
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

type RSMBox(startState:int<rsmState>, finalStates:HashSet<int<rsmState>>, transitions: array<RSMEdges>) =
    let outgoingEdges = Dictionary<int<rsmState>,ResizeArray<RSMEdges>>()
    let incomingEdges = Dictionary<int<rsmState>,ResizeArray<RSMEdges>>()
    let addTransition (transition: RSMEdges) =
        let exists, edges = outgoingEdges.TryGetValue transition.StartState 
        if exists
        then edges.Add transition
        else outgoingEdges.Add (transition.StartState, ResizeArray[|transition|])
        if not <| outgoingEdges.ContainsKey transition.FinalState
        then outgoingEdges.Add(transition.FinalState, ResizeArray<_>())
        
        let exists, edges = incomingEdges.TryGetValue transition.FinalState 
        if exists
        then edges.Add transition
        else incomingEdges.Add (transition.FinalState, ResizeArray[|transition|])
        if not <| incomingEdges.ContainsKey transition.StartState
        then incomingEdges.Add(transition.StartState, ResizeArray<_>())
        
    do Array.iter addTransition transitions        
   
    let transitions = ResizeArray transitions
             
    member this.StartState = startState
    member this.FinalStates = finalStates
    member this.Transitions = transitions
    member this.OutgoingEdges v = outgoingEdges.[v]
    member this.IncomingEdges v = incomingEdges.[v]
    member this.AddTransition t =
        transitions.Add t
        addTransition t
        
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
            |> ResizeArray.iter(
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
                  if edges.Length <= 20
                  then Small edges
                  else
                      let dict = Dictionary<_,ResizeArray<_>>()
                      for edge in edges do
                          let exists, edges = dict.TryGetValue edge.TerminalSymbol
                          if exists
                          then edges.Add edge.State
                          else dict.Add(edge.TerminalSymbol, ResizeArray<_>[|edge.State|])
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
    member this.StatesCount with get () = vertices.Count
        
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
                     for _to in _kvp.Value do
                        yield $"%i{kvp.Key} -> %i{_to} [label = t_%i{_kvp.Key}]"                               
         yield "}"
        }
        |> fun x -> System.IO.File.WriteAllLines(filePath, x)