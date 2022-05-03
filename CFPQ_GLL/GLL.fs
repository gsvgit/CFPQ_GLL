module CFPQ_GLL.GLL

open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open FSharpx.Collections
    
let eval (graph:InputGraph) startVertices (query:RSM) (startStates:array<int<rsmState>>) =    
    let reachableVertices = ResizeArray<_>()
    let descriptorToProcess = System.Collections.Generic.Stack<_>()
    
    let gss = GSS()
    
    let inline addDescriptor (descriptor:Descriptor) =
        if not <| gss.IsThisDescriptorAlreadyHandled descriptor
        then descriptorToProcess.Push descriptor 
        
    startVertices
    |> Array.iter (fun v ->        
        startStates |> Array.iter (fun startState ->
            let gssVertex = gss.AddNewVertex(v, startState)
            Descriptor(v, gssVertex, startState)
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
            
            gss.Pop currentDescriptor.GSSVertex currentDescriptor.InputPosition
            |> ResizeArray.iter (fun gssEdge -> Descriptor(currentDescriptor.InputPosition, gssEdge.GSSVertex, gssEdge.RSMState) |> addDescriptor)
            
        let outgoingTerminalEdgesInGraph = graph.OutgoingTerminalEdges currentDescriptor.InputPosition
        let outgoingCFGEdgesInGraph = graph.OutgoingCFGEdges currentDescriptor.InputPosition
            
        let outgoingNonTerminalEdgesInRSM = query.OutgoingNonTerminalEdges currentDescriptor.RSMState
        let outgoingTerminalEdgesInRSM = query.OutgoingTerminalEdges currentDescriptor.RSMState
        let outgoingCFGEdgesInRSM = query.OutgoingCFGEdges currentDescriptor.RSMState
        
        outgoingNonTerminalEdgesInRSM
        |> Array.iter (fun edge ->
               let edge = unpackRSMNonTerminalEdge edge
               let newGSSVertex, positionsForPops = gss.AddEdge(currentDescriptor.GSSVertex, edge.State, currentDescriptor.InputPosition, edge.NonTerminalSymbolStartState)
               Descriptor(currentDescriptor.InputPosition, newGSSVertex, edge.NonTerminalSymbolStartState)
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun pos -> Descriptor(pos, currentDescriptor.GSSVertex, edge.State) |> addDescriptor)
        )
        
        outgoingTerminalEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingTerminalEdgesInGraph
            |> Array.iter (fun e2 ->
                let graphEdge = unpackInputGraphTerminalEdge e2
                let rsmEdge = unpackRSMTerminalEdge e1
                if graphEdge.TerminalSymbol = rsmEdge.TerminalSymbol
                then Descriptor(graphEdge.Vertex, currentDescriptor.GSSVertex, rsmEdge.State) |> addDescriptor))
            
        outgoingCFGEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingCFGEdgesInGraph
            |> Array.iter (fun e2 ->
                let nextPosition = unpackInputGraphCFGEdge e2
                let nextState = unpackRSMCFGEdge e1
                Descriptor(nextPosition, currentDescriptor.GSSVertex, nextState) |> addDescriptor))
    
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Query processing total time: %A{(System.DateTime.Now - startTime).TotalMilliseconds} milliseconds"
    //printfn $"Total descriptors handled: %A{handledDescriptors.Count}"
    //printfn $"Average throughput: %A{float handledDescriptors.Count / (System.DateTime.Now - startTime).TotalSeconds} descriptors per second."
        
    reachableVertices
