module CFPQ_GLL.RSM

open System
open System.Collections.Generic
open CFPQ_GLL.Common

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

type RsmState (isStart: bool, isFinal: bool) =
    let descriptors = ResizeArray<WeakReference<Descriptor>>()
    let outgoingTerminalEdges = Dictionary<int<terminalSymbol>, HashSet<IRsmState>>()
    let outgoingNonTerminalEdges= Dictionary<IRsmState, HashSet<IRsmState>>()
    let nonTerminalNodes = ResizeArray()
    let mutable rsmBox = None
    new () = RsmState(false,false)

    interface IRsmState with
        member this.OutgoingTerminalEdges = outgoingTerminalEdges
        member this.OutgoingNonTerminalEdges = outgoingNonTerminalEdges
        member this.Descriptors = descriptors
        member this.GetValidDescriptors () =
            let count = descriptors.RemoveAll(fun d -> let isAlive, d= d.TryGetTarget() in not (isAlive && d.IsAlive))
            descriptors |> Seq.map (fun d -> let _,d = d.TryGetTarget() in d)
        member this.IsStart = isStart
        member this.IsFinal = isFinal
        member this.NonTerminalNodes = nonTerminalNodes
        member this.AddTerminalEdge (terminal, targetState) =
            let exists,targetStates = (this:>IRsmState).OutgoingTerminalEdges.TryGetValue terminal
            if exists
            then
                targetStates.Add targetState |> ignore
            else 
                (this:>IRsmState).OutgoingTerminalEdges.Add(terminal, HashSet [|targetState|])
        member this.AddNonTerminalEdge (nonTerminal, targetState) =
            let exists,targetStates = (this:>IRsmState).OutgoingNonTerminalEdges.TryGetValue nonTerminal
            if exists
            then
                targetStates.Add targetState |> ignore
            else 
                (this:>IRsmState).OutgoingNonTerminalEdges.Add(nonTerminal, HashSet [|targetState|])    
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
    let finalStates = HashSet<RsmState>()
    member this.AddState (state:RsmState) =
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
        member this.Nonterminal = nonTerminal

type RSM(boxes:array<RSMBox>, startBox:RSMBox) =
    let startStateOfExtendedRSM = RsmState()

    do
        let originalStartState = startBox.StartState
        let finalState = RsmState(false,true)
        let intermediateState = RsmState()
        startStateOfExtendedRSM.AddNonTerminalEdge(originalStartState, intermediateState)
        // We doesn't use AddTerminalEdge because we don't want to add EOF to ErrorRecoveryLabels
        intermediateState.OutgoingTerminalEdges.Add(EOF, HashSet[|finalState|])
        let box = RSMBox(NonterminalBase "__Extended__")
        box.AddState startStateOfExtendedRSM
        box.AddState intermediateState
        box.AddState finalState

    member this.StartState = startStateOfExtendedRSM
    member this.IsFinalStateForOriginalStartBox state = (startBox :> IRsmBox).FinalStates.Contains state
    member this.OriginalStartState = startBox.StartState

    member this.ToDot filePath =        
        let visited = HashSet<_>()
        
        let rec toDot (v: RsmState) =
            if not <| visited.Contains v
            then
                let added = visited.Add v
                assert added
                seq {
                        if v.IsFinal
                        then
                            if v.IsStart
                            then yield $"%i{v.Id} [shape = doublecircle style = filled fillcolor=green label=%A{v.Box.Nonterminal.Name}]"
                            else yield $"%i{v.Id} [shape = doublecircle]"
                        elif v.IsStart
                        then yield $"%i{v.Id} [style = filled fillcolor=green label=%A{v.Box.Nonterminal.Name}]"
                        for e in v.OutgoingTerminalEdges do
                            for target in e.Value do
                                yield $"%i{v.Id} -> %i{target.Id} [label = %A{e.Key}]"
                                yield! toDot target

                        for e in v.OutgoingNonTerminalEdges do
                            for target in e.Value do
                                yield $"%i{v.Id} -> %i{target.Id} [label = %A{e.Key.Box.Nonterminal.Name}]"
                                yield! toDot target
                    }
            else Seq.empty

        seq {
         yield "digraph g {"
         for box in boxes do
            yield! toDot box.StartState
         yield "}"
        }
        |> fun x -> System.IO.File.WriteAllLines(filePath, x)
