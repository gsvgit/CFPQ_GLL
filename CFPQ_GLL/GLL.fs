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
    | ReachabilityFacts of Dictionary<int<graphVertex>,HashSet<int<graphVertex>>>
    | MatchedRanges of MatchedRanges

let evalFromState (reachableVertices:Dictionary<_,HashSet<_>>) (gss:GSS) (matchedRanges:MatchedRanges) (graph:InputGraph) (startVertices:array<_>) (query:RSM) mode =
    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true
        
    
        
    let descriptorToProcess = Stack<_>()
    
        
    
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
                then reachableVertices.[startPosition].Add(currentDescriptor.InputPosition) |> ignore
            
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
                    if buildSppf then matchedRanges.AddMatchedRange newRange
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
                    if buildSppf then matchedRanges.AddMatchedRange rightSubRange   
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
                   let newRange =
                       if buildSppf
                       then 
                           let rightSubRange =
                               MatchedRange(
                                    matchedRange.InputRange.StartPosition
                                  , matchedRange.InputRange.EndPosition
                                  , currentDescriptor.RSMState
                                  , edge.State
                                  , RangeType.NonTerminal edge.NonTerminalSymbolStartState
                               )
                           matchedRanges.AddMatchedRange rightSubRange
                           let leftSubRange = currentDescriptor.MatchedRange
                           Some <| matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                       else None 
                   Descriptor(matchedRange.InputRange.EndPosition, currentDescriptor.GSSVertex, edge.State, newRange) |> addDescriptor)
        )
        
        let inline handleTerminalEdge (graphEdge:InputGraphTerminalEdge) (rsmEdge:RSMTerminalEdge) =
            let newMatchedRange =                
                if buildSppf
                then
                    let currentlyMatchedRange =
                        MatchedRange(
                            currentDescriptor.InputPosition
                            , graphEdge.Vertex
                            , currentDescriptor.RSMState
                            , rsmEdge.State
                            , RangeType.Terminal rsmEdge.TerminalSymbol)
                        
                    matchedRanges.AddMatchedRange currentlyMatchedRange                    
                    Some <| matchedRanges.AddMatchedRange (currentDescriptor.MatchedRange, currentlyMatchedRange)
                else None                
            Descriptor(graphEdge.Vertex, currentDescriptor.GSSVertex, rsmEdge.State, newMatchedRange) |> addDescriptor
            
        outgoingTerminalEdgesInGraph
        |> Array.iter (fun e2 ->
            let graphEdge = unpackInputGraphTerminalEdge e2
            match outgoingTerminalEdgesInRSM with
            | Small a ->
                a |> Array.iter (fun e1 ->                                    
                    let rsmEdge = unpackRSMTerminalEdge e1
                    if graphEdge.TerminalSymbol = rsmEdge.TerminalSymbol
                    then handleTerminalEdge graphEdge rsmEdge
                    )
            | Big d ->
                let exists, state =  d.TryGetValue graphEdge.TerminalSymbol
                if exists
                then handleTerminalEdge graphEdge (RSMTerminalEdge(state, graphEdge.TerminalSymbol))                
                )
        
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Query processing total time: %A{(System.DateTime.Now - startTime).TotalMilliseconds} milliseconds"

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts reachableVertices
    | AllPaths -> QueryResult.MatchedRanges matchedRanges
    , gss
    

let eval (graph:InputGraph) (startVertices:array<_>) (query:RSM) mode =
    let reachableVertices =
        let d = Dictionary<_,_>(startVertices.Length)
        startVertices
        |> Array.iter (fun v -> d.Add(v, HashSet<_>()))
        d
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    fst <| evalFromState reachableVertices gss matchedRanges (graph:InputGraph) (startVertices:array<_>) (query:RSM) mode

let evalParallel blockSize (graph:InputGraph) startVertices (query:RSM) mode =
    Array.chunkBySize blockSize startVertices
    |> Array.Parallel.map (fun startVertices -> eval graph startVertices query mode)
    |> Array.fold
           (fun state item ->
            match (state,item) with
            | QueryResult.ReachabilityFacts s1, QueryResult.ReachabilityFacts s2 ->
                for kvp in s2 do s1.Add(kvp.Key, kvp.Value)
                QueryResult.ReachabilityFacts s1 
            | QueryResult.MatchedRanges s1, QueryResult.MatchedRanges s2 ->
                s1.UnionWith s2
                QueryResult.MatchedRanges s1
            | _ -> failwith "Inconsistent query result!"
            )
           (match mode with
            | ReachabilityOnly -> QueryResult.ReachabilityFacts <| Dictionary()
            | AllPaths -> QueryResult.MatchedRanges <| MatchedRanges())