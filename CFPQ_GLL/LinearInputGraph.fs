module CFPQ_GLL.LinearInputGraph

open System
open System.Collections.Generic
open CFPQ_GLL.Common

type LinearInputGraphVertexBase<'token when 'token: equality> (id:int32<inputGraphVertex>, epsilon) =
    let mutable outgoingEdge : Option<ITerminal<'token> * TerminalEdgeTarget<'token>> = None
    let descriptors = HashSet<WeakReference<Descriptor<'token>>>()
    let terminalNodes = Dictionary<IInputGraphVertex<'token>, Dictionary<'token, WeakReference<ITerminalNode<'token>>>>()
    let nonTerminalNodes = Dictionary<IInputGraphVertex<'token>, Dictionary<IRsmState<'token>, WeakReference<INonTerminalNode<'token>>>>()
    let rangeNodes = Dictionary<MatchedRange<'token>, WeakReference<IRangeNode<'token>>>()        
    let intermediateNodes = Dictionary<MatchedRange<'token>, Dictionary<MatchedRange<'token>, WeakReference<IIntermediateNode<'token>>>>()
    override this.GetHashCode() = int id
    
    member this.AddOutgoingEdge (terminal, target) =
        match outgoingEdge with
        | None -> outgoingEdge <- Some (terminal, target)
        | Some x -> failwithf $"Edge exists: %A{x}"
    
    member this.OutgoingEdge =
            match outgoingEdge with
            | Some v -> v
            | None -> failwith "Unexpected end of input"

    interface IInputGraphVertex<'token> with
        member this.ForAllOutgoingEdges (currentDescriptor:Descriptor<'token>) handleTerminalEdge handleEpsilonEdge =
            let (currentTerminal,targetVertex) as outgoingTerminalEdgeInGraph = this.OutgoingEdge            
            let errorRecoveryEdges =
                let errorRecoveryEdges = Dictionary()
                let coveredByCurrentTerminal =
                    let exists, s = currentDescriptor.RsmState.OutgoingTerminalEdges.TryGetValue currentTerminal.Token
                    if exists then s else HashSet<_>()
                for terminal in currentDescriptor.RsmState.ErrorRecoveryLabels do
                    let coveredByTerminal = HashSet(currentDescriptor.RsmState.OutgoingTerminalEdges[terminal])
                    coveredByTerminal.ExceptWith coveredByCurrentTerminal
                    if terminal <> currentTerminal.Token && coveredByTerminal.Count > 0
                    then
                        errorRecoveryEdges.Add(terminal, TerminalEdgeTarget(currentDescriptor.InputPosition, 1<weight>))
                errorRecoveryEdges.Add(epsilon, TerminalEdgeTarget(targetVertex.TargetVertex, 1<weight>))
                errorRecoveryEdges

            for kvp in errorRecoveryEdges do
                if kvp.Key = epsilon then
                    handleEpsilonEdge kvp.Value
                else
                    handleTerminalEdge kvp.Key kvp.Value
                                
            assert (currentTerminal.Token <> epsilon)
            handleTerminalEdge currentTerminal.Token targetVertex

        member this.Id = id        
        member this.Descriptors = descriptors
        member this.GetValidDescriptors() =
            let count = descriptors.RemoveWhere (fun d -> let isAlive, d = d.TryGetTarget () in not (isAlive && d.IsAlive))
            descriptors
            |> Seq.map (fun d -> let isAlive, d = d.TryGetTarget () in d)
        member this.TerminalNodes = terminalNodes
        member this.NonTerminalNodesStartedHere = nonTerminalNodes 
        member this.RangeNodes = rangeNodes
        member this.IntermediateNodes = intermediateNodes
  