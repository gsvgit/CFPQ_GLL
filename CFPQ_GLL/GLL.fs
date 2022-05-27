module CFPQ_GLL.GLL

open System.Collections.Generic
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.SPPF
open FSharpx.Collections


type Mode =
    | ReachabilityOnly
    | AllPaths

[<RequireQualifiedAccess>]
type QueryResult =
    | ReachabilityFacts of HashSet<int<graphVertex>*int<graphVertex>>
    | MatchedRanges of MatchedRanges

let eval (graph:InputGraph) startVertices (query:RSM) mode =
    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true
        
    let reachableVertices = ResizeArray<_>()
    let descriptorToProcess = Stack<_>()
    
    let gss = GSS()
    let matchedRanges = MatchedRanges()    
    
    let inline addDescriptor (descriptor:Descriptor) =
        if not <| gss.IsThisDescriptorAlreadyHandled descriptor
        then descriptorToProcess.Push descriptor 
        
    startVertices
    |> Array.iter (fun v ->        
        let gssVertex = gss.AddNewVertex(v, query.StartState)            
        Descriptor(v, gssVertex, query.StartState, None)
        |> descriptorToProcess.Push
        )
    
    let handleDescriptor (currentDescriptor:Descriptor) =
       
        gss.AddDescriptorToHandled currentDescriptor
                
        if query.IsFinalState currentDescriptor.RSMState                        
        then
            if (not buildSppf) && query.IsFinalStateForOriginalStartBox currentDescriptor.RSMState
            then
                let startPosition = currentDescriptor.GSSVertex.InputPosition
                if Array.contains startPosition startVertices
                then reachableVertices.Add (startPosition, currentDescriptor.InputPosition)
            
            let matchedRange =
                match currentDescriptor.MatchedRange with             
                | None ->
                    let newRange =
                        MatchedRange(
                               currentDescriptor.InputPosition
                             , currentDescriptor.InputPosition
                             , currentDescriptor.RSMState
                             , currentDescriptor.RSMState
                             , RangeType.EpsilonNonTerminal currentDescriptor.GSSVertex.RSMState
                        )
                    if buildSppf then matchedRanges.AddMatchedRange(newRange)
                    newRange
                | Some range -> range
                                                
            gss.Pop(currentDescriptor, matchedRange)            
            |> ResizeArray.iter (
                fun gssEdge ->                
                    let leftSubRange = gssEdge.Info
                    let rightSubRange =           
                        MatchedRange(
                            currentDescriptor.GSSVertex.InputPosition
                          , currentDescriptor.InputPosition
                          , match gssEdge.Info with
                            | None -> gssEdge.GSSVertex.RSMState
                            | Some v -> v.RSMRange.EndPosition
                          , gssEdge.RSMState
                          , RangeType.NonTerminal currentDescriptor.GSSVertex.RSMState
                        )
                    if buildSppf then matchedRanges.AddMatchedRange(rightSubRange)   
                    let newRange =
                        if buildSppf
                        then Some <| matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                        else None
                    Descriptor(currentDescriptor.InputPosition, gssEdge.GSSVertex, gssEdge.RSMState, newRange)
                    |> addDescriptor
                )
            
        let outgoingTerminalEdgesInGraph = graph.OutgoingTerminalEdges currentDescriptor.InputPosition        
            
        let outgoingNonTerminalEdgesInRSM = query.OutgoingNonTerminalEdges currentDescriptor.RSMState
        let outgoingTerminalEdgesInRSM = query.OutgoingTerminalEdges currentDescriptor.RSMState       
        
        outgoingNonTerminalEdgesInRSM
        |> Array.iter (fun edge ->
               let edge = unpackRSMNonTerminalEdge edge
               let newGSSVertex, positionsForPops =
                    gss.AddEdge(currentDescriptor.GSSVertex
                                , edge.State
                                , currentDescriptor.InputPosition
                                , edge.NonTerminalSymbolStartState
                                , currentDescriptor.MatchedRange)
               
               Descriptor(currentDescriptor.InputPosition, newGSSVertex, edge.NonTerminalSymbolStartState, None)
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun matchedRange ->                   
                   let rightSubRange =
                       MatchedRange(
                            matchedRange.InputRange.StartPosition
                          , matchedRange.InputRange.EndPosition
                          , currentDescriptor.RSMState
                          , edge.State
                          , RangeType.NonTerminal edge.NonTerminalSymbolStartState
                       )
                   if buildSppf then matchedRanges.AddMatchedRange(rightSubRange)
                   let leftSubRange = currentDescriptor.MatchedRange
                   let newRange =
                       if buildSppf
                       then Some <| matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                       else None 
                   Descriptor(matchedRange.InputRange.EndPosition, currentDescriptor.GSSVertex, edge.State, newRange) |> addDescriptor)
        )
        
        outgoingTerminalEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingTerminalEdgesInGraph
            |> Array.iter (fun e2 ->
                let graphEdge = unpackInputGraphTerminalEdge e2
                let rsmEdge = unpackRSMTerminalEdge e1
                if graphEdge.TerminalSymbol = rsmEdge.TerminalSymbol
                then
                    let currentlyMatchedRange =
                        MatchedRange(
                            currentDescriptor.InputPosition
                            , graphEdge.Vertex
                            , currentDescriptor.RSMState
                            , rsmEdge.State
                            , RangeType.Terminal rsmEdge.TerminalSymbol)
                        
                    if buildSppf then matchedRanges.AddMatchedRange(currentlyMatchedRange)    
                    let newMatchedRange =
                        if buildSppf
                        then Some <| matchedRanges.AddMatchedRange (currentDescriptor.MatchedRange, currentlyMatchedRange)
                        else None
                    Descriptor(graphEdge.Vertex, currentDescriptor.GSSVertex, rsmEdge.State, newMatchedRange) |> addDescriptor))
    
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Query processing total time: %A{(System.DateTime.Now - startTime).TotalMilliseconds} milliseconds"

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts (HashSet reachableVertices)
    | AllPaths -> QueryResult.MatchedRanges matchedRanges

let evalParallel blockSize (graph:InputGraph) startVertices (query:RSM) mode =
    Array.chunkBySize blockSize startVertices
    |> Array.Parallel.map (fun startVertices -> eval graph startVertices query mode)
    |> Array.fold
           (fun state item ->
            match (state,item) with
            | QueryResult.ReachabilityFacts s1, QueryResult.ReachabilityFacts s2 ->
                s1.UnionWith s2
                QueryResult.ReachabilityFacts s1 
            | QueryResult.MatchedRanges s1, QueryResult.MatchedRanges s2 ->
                s1.UnionWith s2
                QueryResult.MatchedRanges s1
            | _ -> failwith "Inconsistent query result!"
            )
           (match mode with
            | ReachabilityOnly -> QueryResult.ReachabilityFacts <| HashSet()
            | AllPaths -> QueryResult.MatchedRanges <| MatchedRanges())