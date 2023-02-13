module CFPQ_GLL.RSM

open System.Collections.Generic
open CFPQ_GLL.Common

type RSMBox(nonTerminal:INonterminal) =
    let nonTerminal = nonTerminal
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
