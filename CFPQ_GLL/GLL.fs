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
        MatchedRangeWithNode
            (
                    -1<inputGraphVertex>
                  , -1<inputGraphVertex>
                  , -1<rsmState>
                  , -1<rsmState>                  
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
            if (*not buildSppf) &&*) query.IsFinalStateForOriginalStartBox currentDescriptor.RSMState
            then
                let startPosition = currentDescriptor.GSSVertex.InputPosition
                if startVertices.Contains startPosition
                then
                    reachableVertices.[startPosition].Add(currentDescriptor.InputPosition) |> ignore                    
            
            let matchedRange =
                match currentDescriptor.MatchedRange.Node with             
                | None ->
                    let currentlyCreatedNode = EpsilonNode (currentDescriptor.InputPosition, currentDescriptor.GSSVertex.RSMState)
                    let matchedRange = MatchedRange (
                             currentDescriptor.InputPosition
                             , currentDescriptor.InputPosition
                             , currentDescriptor.RSMState
                             , currentDescriptor.RSMState)
                    
                    let newRangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.EpsilonNode currentlyCreatedNode)                    
                    MatchedRangeWithNode(matchedRange, newRangeNode)
                    
                | _ -> currentDescriptor.MatchedRange
                                                
            gss.Pop(currentDescriptor, matchedRange)            
            |> ResizeArray.iter (
                fun gssEdge ->
                    let newRange =
                        if buildSppf
                        then 
                            let leftSubRange = gssEdge.Info
                            let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(Range(currentDescriptor.GSSVertex.InputPosition, currentDescriptor.InputPosition), currentDescriptor.GSSVertex.RSMState, query)
                            let rightSubRange =
                                let matchedRange =
                                  MatchedRange (
                                      currentDescriptor.GSSVertex.InputPosition
                                      , currentDescriptor.InputPosition
                                      , match gssEdge.Info.Node with
                                        | None -> gssEdge.GSSVertex.RSMState
                                        | _ -> gssEdge.Info.Range.RSMRange.EndPosition
                                      , gssEdge.RSMState)
                                let rangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                                MatchedRangeWithNode(matchedRange, rangeNode)
                            matchedRanges.AddIntermediateNode (leftSubRange, rightSubRange)                            
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
                           let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(matchedRange.Range.InputRange ,edge.NonTerminalSymbolStartState,query)
                           let rightSubRange =
                               let matchedRange =
                                  MatchedRange(
                                      matchedRange.Range.InputRange.StartPosition
                                      , matchedRange.Range.InputRange.EndPosition
                                      , currentDescriptor.RSMState
                                      , edge.State)
                               let rangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                               MatchedRangeWithNode(matchedRange, rangeNode)                           
                           let leftSubRange = currentDescriptor.MatchedRange
                           matchedRanges.AddIntermediateNode(leftSubRange, rightSubRange)
                       else emptyRange 
                   Descriptor(matchedRange.Range.InputRange.EndPosition, currentDescriptor.GSSVertex, edge.State, newRange) |> addDescriptor)
        )
        // inline
        let  handleTerminalEdge (graphEdge:InputGraphEdge) (rsmEdge:RSMTerminalEdge) =
            let newMatchedRange =                
                if buildSppf
                then
                    let currentlyCreatedNode = matchedRanges.AddTerminalNode(Range(currentDescriptor.InputPosition, graphEdge.TargetVertex), rsmEdge.TerminalSymbol)
                    let currentlyMatchedRange =
                        let matchedRange =
                            MatchedRange(
                                currentDescriptor.InputPosition
                                , graphEdge.TargetVertex
                                , currentDescriptor.RSMState
                                , rsmEdge.State
                            )
                        let rangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.TerminalNode currentlyCreatedNode)
                        MatchedRangeWithNode(matchedRange, rangeNode)                                            
                    matchedRanges.AddIntermediateNode (currentDescriptor.MatchedRange, currentlyMatchedRange)
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