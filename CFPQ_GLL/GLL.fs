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
type QueryResult<'inputVertex when 'inputVertex: equality> =
    | ReachabilityFacts of Dictionary<'inputVertex,HashSet<'inputVertex>>
    | MatchedRanges of MatchedRanges<'inputVertex>

let evalFromState<'inputVertex when 'inputVertex: equality>
        (reachableVertices:Dictionary<_,HashSet<_>>)
        (gss:GSS<'inputVertex>)
        (matchedRanges:MatchedRanges<'inputVertex>)
        (graph:IInputGraph<'inputVertex>)
        (startVertices:array<'inputVertex>)
        (query:RSM) mode =
    
    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true
        
    let descriptorToProcess = Stack<_>()
    
    let inline addDescriptor (descriptor:Descriptor<'inputVertex>) =
        if not <| gss.IsThisDescriptorAlreadyHandled descriptor
        then descriptorToProcess.Push descriptor 
        
    startVertices
    |> Array.iter (fun v ->        
        let gssVertex = gss.AddNewVertex(v, query.StartState)            
        Descriptor(v, gssVertex, query.StartState, None)
        |> descriptorToProcess.Push
        )
    
    let handleDescriptor (currentDescriptor:Descriptor<'inputVertex>) =
       
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
                        MatchedRangeWithType(
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
                    let newRange =
                        if buildSppf
                        then 
                            let leftSubRange = gssEdge.Info
                            let rightSubRange =           
                                MatchedRangeWithType(
                                    currentDescriptor.GSSVertex.InputPosition
                                  , currentDescriptor.InputPosition
                                  , match gssEdge.Info with
                                    | None -> gssEdge.GSSVertex.RSMState
                                    | Some v -> v.Range.RSMRange.EndPosition
                                  , gssEdge.RSMState
                                  , RangeType.NonTerminal currentDescriptor.GSSVertex.RSMState
                                )
                            matchedRanges.AddMatchedRange rightSubRange   
                            Some <| matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                        else None
                    Descriptor(currentDescriptor.InputPosition, gssEdge.GSSVertex, gssEdge.RSMState, newRange)
                    |> addDescriptor
                )
            
        let outgoingTerminalEdgesInGraph = graph.GetOutgoingEdges currentDescriptor.InputPosition        
            
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
                               MatchedRangeWithType(
                                    matchedRange.Range.InputRange.StartPosition
                                  , matchedRange.Range.InputRange.EndPosition
                                  , currentDescriptor.RSMState
                                  , edge.State
                                  , RangeType.NonTerminal edge.NonTerminalSymbolStartState
                               )
                           matchedRanges.AddMatchedRange rightSubRange
                           let leftSubRange = currentDescriptor.MatchedRange
                           Some <| matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                       else None 
                   Descriptor(matchedRange.Range.InputRange.EndPosition, currentDescriptor.GSSVertex, edge.State, newRange) |> addDescriptor)
        )
        
        let inline handleTerminalEdge (graphEdge:InputGraphEdge<'inputVertex>) (rsmEdge:RSMTerminalEdge) =
            let newMatchedRange =                
                if buildSppf
                then
                    let currentlyMatchedRange =
                        MatchedRangeWithType(
                            currentDescriptor.InputPosition
                            , graphEdge.TargetVertex
                            , currentDescriptor.RSMState
                            , rsmEdge.State
                            , RangeType.Terminal rsmEdge.TerminalSymbol)
                        
                    matchedRanges.AddMatchedRange currentlyMatchedRange                    
                    Some <| matchedRanges.AddMatchedRange (currentDescriptor.MatchedRange, currentlyMatchedRange)
                else None                
            Descriptor(graphEdge.TargetVertex, currentDescriptor.GSSVertex, rsmEdge.State, newMatchedRange) |> addDescriptor
        
        let handleEdge (graphEdge: InputGraphEdge<'inputVertex>) =
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
                    
        InputGraphEdge (EOF, currentDescriptor.InputPosition)
        |> handleEdge
                                  
        outgoingTerminalEdgesInGraph
        |> ResizeArray.iter handleEdge            
            
        
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Query processing total time: %A{(System.DateTime.Now - startTime).TotalMilliseconds} milliseconds"

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts reachableVertices
    | AllPaths -> QueryResult.MatchedRanges matchedRanges
    , gss

let eval<'inputVertex when 'inputVertex: equality> (graph:IInputGraph<'inputVertex>) (startVertices:array<_>) (query:RSM) mode =
    let reachableVertices =
        let d = Dictionary<_,_>(startVertices.Length)
        startVertices
        |> Array.iter (fun v -> d.Add(v, HashSet<_>()))
        d
    let gss = GSS()
    let matchedRanges = MatchedRanges(query)
    fst <| evalFromState reachableVertices gss matchedRanges (graph:IInputGraph<'inputVertex>) (startVertices:array<_>) (query:RSM) mode

let evalParallel blockSize (graph:IInputGraph<'inputVertex>) startVertices (query:RSM) mode =
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
            | AllPaths -> QueryResult.MatchedRanges <| MatchedRanges query)