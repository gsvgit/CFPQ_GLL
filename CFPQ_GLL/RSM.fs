module CFPQ_GLL.RSM

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph

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
    val State : IRsmState
    val NonTerminalSymbolStartState : IRsmState
    new (state, nonTerminalSymbolStartState) = {State = state; NonTerminalSymbolStartState = nonTerminalSymbolStartState}    

type TerminalEdgesStorage =
    | Small of array<RSMTerminalEdge>
    | Big of Dictionary<int<terminalSymbol>,ResizeArray<int<rsmState>>>
    
type RsmVertex (isStart: bool, isFinal: bool) =
    let descriptors = ResizeArray<Descriptor>()
    let outgoingTerminalEdges = Dictionary<int<terminalSymbol>, HashSet<IRsmState>>()
    let outgoingNonTerminalEdges= Dictionary<IRsmState, HashSet<IRsmState>>()
    let nonTerminalNodes = ResizeArray()
    let mutable rsmBox = None
    new () = RsmVertex(false,false)        
    
    interface IRsmState with
        member this.OutgoingTerminalEdges = outgoingTerminalEdges
        member this.OutgoingNonTerminalEdges = outgoingNonTerminalEdges
        member this.Descriptors = descriptors
        member this.IsStart = isStart
        member this.IsFinal = isFinal
        member this.NonTerminalNodes = nonTerminalNodes
        member this.Box
            with get () =
                    match rsmBox with
                    | None -> failwith "Rsm state without rsm box."
                    | Some b -> b
            and set v = rsmBox <- Some v 
    
[<Struct>]
type RSMVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<RSMTerminalEdge>    
    val OutgoingNonTerminalEdges: ResizeArray<RSMNonTerminalEdge>
    new (terminalEdges, nonTerminalEdges) =
        {
            OutgoingTerminalEdges = terminalEdges            
            OutgoingNonTerminalEdges = nonTerminalEdges
        }

type RSMBox() =
    let mutable startState = None
    let finalStates = HashSet<IRsmState>()    
    member this.AddState (state:IRsmState) =
        state.Box <- this
        if state.IsFinal
        then
            let added = finalStates.Add state
            assert added
        if state.IsStart
        then startState <- Some state
    member this.StartState
        with get () =
            match startState with
            | None -> failwith "Rsm without start state."
            | Some s -> s 
    
    interface IRsmBox with
        member this.FinalStates = finalStates
        
type RSM(boxes:array<RSMBox>, startBox:RSMBox) =    
    let finalStates = HashSet<_>()
    let finalStatesForBox = Dictionary<int<rsmState>,ResizeArray<_>>()
    let startStateOfExtendedRSM = RsmVertex() :> IRsmState
        
    let extensionBox =
        let originalStartState = startBox.StartState        
        let finalState = RsmVertex() :> IRsmState
        let intermediateState = RsmVertex() :> IRsmState        
        startStateOfExtendedRSM.OutgoingNonTerminalEdges.Add(originalStartState, HashSet[|intermediateState|])
        intermediateState.OutgoingTerminalEdges.Add(EOF, HashSet[|finalState|]) 
        let box = RSMBox()
        box.AddState startStateOfExtendedRSM
        box.AddState intermediateState
        box.AddState finalState
    
    member this.StartState = startStateOfExtendedRSM
    member this.IsFinalStateForOriginalStartBox state = (startBox :> IRsmBox).FinalStates.Contains state
    member this.OriginalStartState = startBox.StartState

    (*        
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
        
        *)