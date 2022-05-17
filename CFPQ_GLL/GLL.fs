module CFPQ_GLL.GLL

open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.SPPF
open FSharpx.Collections
    
let eval (graph:InputGraph) startVertices (query:RSM) (startStates:array<int<rsmState>>) =    
    let reachableVertices = ResizeArray<_>()
    let descriptorToProcess = System.Collections.Generic.Stack<_>()
    
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    
    let inline addDescriptor (descriptor:Descriptor) =
        if not <| gss.IsThisDescriptorAlreadyHandled descriptor
        then descriptorToProcess.Push descriptor 
        
    startVertices
    |> Array.iter (fun v ->        
        startStates
        |> Array.iter (fun startState ->
            let gssVertex = gss.AddNewVertex(v, startState)
            Descriptor(v, gssVertex, startState, None)
            |> descriptorToProcess.Push
            )
        )
    
    let handleDescriptor (currentDescriptor:Descriptor) =
        
        gss.AddDescriptorToHandled currentDescriptor
                
        if query.IsFinalState currentDescriptor.RSMState                        
        then
            let startPosition = currentDescriptor.GSSVertex.InputPosition
            if Array.contains startPosition startVertices
            then reachableVertices.Add (startPosition, currentDescriptor.InputPosition)
            
            gss.Pop currentDescriptor
            |> ResizeArray.iter (fun gssEdge ->
                let newMatchedRange =
                    let leftSubRange = 
                    let rightSubRange = 
                Descriptor(currentDescriptor.InputPosition, gssEdge.GSSVertex, gssEdge.RSMState) |> addDescriptor)
            
        let outgoingTerminalEdgesInGraph = graph.OutgoingTerminalEdges currentDescriptor.InputPosition
        let outgoingCFGEdgesInGraph = graph.OutgoingCFGEdges currentDescriptor.InputPosition
            
        let outgoingNonTerminalEdgesInRSM = query.OutgoingNonTerminalEdges currentDescriptor.RSMState
        let outgoingTerminalEdgesInRSM = query.OutgoingTerminalEdges currentDescriptor.RSMState
        let outgoingCFGEdgesInRSM = query.OutgoingCFGEdges currentDescriptor.RSMState
        
        outgoingNonTerminalEdgesInRSM
        |> Array.iter (fun edge ->
               let edge = unpackRSMNonTerminalEdge edge
               let newGSSVertex, positionsForPops = gss.AddEdge(currentDescriptor.GSSVertex, edge.State, currentDescriptor.InputPosition, edge.NonTerminalSymbolStartState)
               Descriptor(currentDescriptor.InputPosition, newGSSVertex, edge.NonTerminalSymbolStartState, None)
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun pos ->
                   let rightSubRange = MatchedRange(currentDescriptor.InputPosition, pos.InputPosition, currentDescriptor.RSMState, pos.RSMState)
                   let leftSubRange = currentDescriptor.MatchedRange
                   let newRange =
                       match leftSubRange with
                       | Some leftSubRange -> matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                       | None ->
                           matchedRanges.AddMatchedRange rightSubRange |> ignore
                           rightSubRange
                   Descriptor(pos.InputPosition, currentDescriptor.GSSVertex, edge.State, Some newRange) |> addDescriptor)
        )
        
        outgoingTerminalEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingTerminalEdgesInGraph
            |> Array.iter (fun e2 ->
                let graphEdge = unpackInputGraphTerminalEdge e2
                let rsmEdge = unpackRSMTerminalEdge e1
                if graphEdge.TerminalSymbol = rsmEdge.TerminalSymbol
                then
                    let currentlyMatchedRange = MatchedRange(currentDescriptor.InputPosition, graphEdge.Vertex, currentDescriptor.RSMState, rsmEdge.State)
                    let newMatchedRange =
                        match currentDescriptor.MatchedRange with
                        Some range -> matchedRanges.AddMatchedRange (range, currentlyMatchedRange)
                        | None -> currentlyMatchedRange
                    Descriptor(graphEdge.Vertex, currentDescriptor.GSSVertex, rsmEdge.State, Some newMatchedRange) |> addDescriptor))
            
        outgoingCFGEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingCFGEdgesInGraph
            |> Array.iter (fun e2 ->
                let nextPosition = unpackInputGraphCFGEdge e2
                let nextState = unpackRSMCFGEdge e1
                let currentlyMatchedRange = MatchedRange(currentDescriptor.InputPosition, nextPosition, currentDescriptor.RSMState, nextState)
                matchedRanges.AddMatchedRange currentlyMatchedRange |> ignore
                let newMatchedRange =
                    match currentDescriptor.MatchedRange with
                    Some range -> matchedRanges.AddMatchedRange (range, currentlyMatchedRange)
                    | None -> currentlyMatchedRange
                Descriptor(nextPosition, currentDescriptor.GSSVertex, nextState, Some newMatchedRange) |> addDescriptor))
    
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Query processing total time: %A{(System.DateTime.Now - startTime).TotalMilliseconds} milliseconds"
        
    reachableVertices
