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
    | ReachabilityFacts of Dictionary<int<inputGraphVertex>,HashSet<int<inputGraphVertex>>>
    | MatchedRanges of MatchedRanges

let evalFromState
        (reachableVertices:Dictionary<_,HashSet<_>>)
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (graph:IInputGraph)
        (startVertices:HashSet<int<inputGraphVertex>>)
        (query:RSM) mode =
    
    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true
        
    let descriptorToProcess = Stack<_>()
    
    let inline addDescriptor (descriptor:Descriptor) =
        if not <| gss.IsThisDescriptorAlreadyHandled descriptor
        then descriptorToProcess.Push descriptor 

    let emptyRange =
        MatchedRangeWithType
            (
                    -1<inputGraphVertex>
                  , -1<inputGraphVertex>
                  , -1<rsmState>
                  , -1<rsmState>
                  , RangeType.Empty
               )

    startVertices
    |> Seq.iter (fun v ->        
        let gssVertex = gss.AddNewVertex(v, query.StartState)            
        Descriptor(v, gssVertex, query.StartState, emptyRange)
        |> descriptorToProcess.Push
        )
    
    let handleDescriptor (currentDescriptor:Descriptor) =
       
        gss.AddDescriptorToHandled currentDescriptor
                
        if query.IsFinalState currentDescriptor.RSMState                        
        then
            if (not buildSppf) && query.IsFinalStateForOriginalStartBox currentDescriptor.RSMState
            then
                let startPosition = currentDescriptor.GSSVertex.InputPosition
                if startVertices.Contains startPosition
                then reachableVertices.[startPosition].Add(currentDescriptor.InputPosition) |> ignore
            
            let matchedRange =
                match currentDescriptor.MatchedRange.RangeType with             
                | RangeType.Empty ->
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
                | _ -> currentDescriptor.MatchedRange
                                                
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
                                  , match gssEdge.Info.RangeType with
                                    | RangeType.Empty -> gssEdge.GSSVertex.RSMState
                                    | _ -> gssEdge.Info.Range.RSMRange.EndPosition
                                  , gssEdge.RSMState
                                  , RangeType.NonTerminal currentDescriptor.GSSVertex.RSMState
                                )
                            matchedRanges.AddMatchedRange rightSubRange   
                            matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                        else emptyRange
                    Descriptor(currentDescriptor.InputPosition, gssEdge.GSSVertex, gssEdge.RSMState, newRange)
                    |> addDescriptor
                )
            
        let outgoingTerminalEdgesInGraph = graph.GetOutgoingEdges currentDescriptor.InputPosition        
            
        let outgoingNonTerminalEdgesInRSM = query.OutgoingNonTerminalEdges currentDescriptor.RSMState
        let outgoingTerminalEdgesInRSM = query.OutgoingTerminalEdges currentDescriptor.RSMState       
        
        outgoingNonTerminalEdgesInRSM
        |> Array.iter (fun edge ->
               let newGSSVertex, positionsForPops =
                    gss.AddEdge(currentDescriptor.GSSVertex
                                , edge.State
                                , currentDescriptor.InputPosition
                                , edge.NonTerminalSymbolStartState
                                , currentDescriptor.MatchedRange)
               
               Descriptor(currentDescriptor.InputPosition, newGSSVertex, edge.NonTerminalSymbolStartState, emptyRange)
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
                           matchedRanges.AddMatchedRange(leftSubRange, rightSubRange)
                       else emptyRange 
                   Descriptor(matchedRange.Range.InputRange.EndPosition, currentDescriptor.GSSVertex, edge.State, newRange) |> addDescriptor)
        )
        
        let inline handleTerminalEdge (graphEdge:InputGraphEdge) (rsmEdge:RSMTerminalEdge) =
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
                    matchedRanges.AddMatchedRange (currentDescriptor.MatchedRange, currentlyMatchedRange)
                else emptyRange                
            Descriptor(graphEdge.TargetVertex, currentDescriptor.GSSVertex, rsmEdge.State, newMatchedRange) |> addDescriptor
        
        let handleEdge (graphEdge: InputGraphEdge) =
            match outgoingTerminalEdgesInRSM with
            | Small a ->
                a |> Array.iter (fun rsmEdge ->
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
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts reachableVertices
    | AllPaths -> QueryResult.MatchedRanges matchedRanges

let eval<'inputVertex when 'inputVertex: equality> (graph:IInputGraph) (startVertices:HashSet<_>) (query:RSM) mode =
    let reachableVertices =
        let d = Dictionary<_,_>(startVertices.Count)
        startVertices
        |> Seq.iter (fun v -> d.Add(v, HashSet<_>()))
        d
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    evalFromState reachableVertices gss matchedRanges (graph:IInputGraph) (startVertices:HashSet<_>) (query:RSM) mode

let evalParallel blockSize (graph:IInputGraph) startVertices (query:RSM) mode =
    Array.chunkBySize blockSize startVertices
    |> Array.Parallel.map (fun startVertices -> eval graph (HashSet<_> startVertices) query mode)
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
            | AllPaths -> QueryResult.MatchedRanges <| MatchedRanges () )