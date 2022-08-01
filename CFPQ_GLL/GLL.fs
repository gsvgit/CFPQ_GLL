module CFPQ_GLL.GLL

open System.Collections.Generic
open CFPQ_GLL.Common
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
    | ReachabilityFacts of Dictionary<IInputGraphVertex,HashSet<IInputGraphVertex>>
    | MatchedRanges of MatchedRanges

let evalFromState
        (reachableVertices:Dictionary<_,HashSet<_>>)
        (gss:GSS)
        (matchedRanges:MatchedRanges)        
        (startVertices:HashSet<IInputGraphVertex>)
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
                    Unchecked.defaultof<IInputGraphVertex>
                  , Unchecked.defaultof<IInputGraphVertex>
                  , Unchecked.defaultof<IRsmState>
                  , Unchecked.defaultof<IRsmState>
               )

    startVertices
    |> Seq.iter (fun v ->        
        let gssVertex = gss.AddNewVertex(v, query.StartState)            
        Descriptor(query.StartState, v, gssVertex, emptyRange)
        |> descriptorToProcess.Push
        )
    
    let handleDescriptor (currentDescriptor:Descriptor) =
       
        gss.AddDescriptorToHandled currentDescriptor
                
        if currentDescriptor.RsmState.IsFinal
        then
            if (not buildSppf) && query.IsFinalStateForOriginalStartBox currentDescriptor.RsmState
            then
                let startPosition = currentDescriptor.GssVertex.InputPosition
                if startVertices.Contains startPosition
                then
                    reachableVertices.[startPosition].Add(currentDescriptor.InputPosition) |> ignore                    
            
            let matchedRange =
                match currentDescriptor.MatchedRange.Node with             
                | None ->
                    let currentlyCreatedNode = EpsilonNode (currentDescriptor.InputPosition, currentDescriptor.GssVertex.RsmState)
                    let matchedRange = MatchedRange (
                             currentDescriptor.InputPosition
                             , currentDescriptor.InputPosition
                             , currentDescriptor.RsmState
                             , currentDescriptor.RsmState)
                    
                    let newRangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.EpsilonNode currentlyCreatedNode)                    
                    MatchedRangeWithNode(matchedRange, newRangeNode)
                    
                | _ -> currentDescriptor.MatchedRange
                                                
            gss.Pop(currentDescriptor, matchedRange)            
            |> ResizeArray.iter (
                fun gssEdge ->
                    let newRange =
                        if buildSppf
                        then 
                            let leftSubRange = gssEdge.MatchedRange
                            let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(Range(currentDescriptor.GssVertex.InputPosition, currentDescriptor.InputPosition), currentDescriptor.GssVertex.RsmState)
                            let rightSubRange =
                                let matchedRange =
                                  MatchedRange (
                                      currentDescriptor.GssVertex.InputPosition
                                      , currentDescriptor.InputPosition
                                      , match gssEdge.MatchedRange.Node with
                                        | None -> gssEdge.GssVertex.RsmState
                                        | _ -> gssEdge.MatchedRange.Range.RSMRange.EndPosition
                                      , gssEdge.RsmState)
                                let rangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                                MatchedRangeWithNode(matchedRange, rangeNode)
                            matchedRanges.AddIntermediateNode (leftSubRange, rightSubRange)                            
                        else emptyRange
                    Descriptor(gssEdge.RsmState, currentDescriptor.InputPosition, gssEdge.GssVertex, newRange)
                    |> addDescriptor
                )
            
        let outgoingTerminalEdgesInGraph = currentDescriptor.InputPosition.OutgoingEdges
            
        let outgoingNonTerminalEdgesInRSM = currentDescriptor.RsmState.OutgoingNonTerminalEdges
        let outgoingTerminalEdgesInRSM = currentDescriptor.RsmState.OutgoingTerminalEdges       
        
        for kvp in outgoingNonTerminalEdgesInRSM do 
            for finalState in kvp.Value do               
               let newGSSVertex, positionsForPops =
                    gss.AddEdge(currentDescriptor.GssVertex
                                , finalState
                                , currentDescriptor.InputPosition
                                , kvp.Key
                                , currentDescriptor.MatchedRange)
               
               Descriptor(kvp.Key, currentDescriptor.InputPosition, newGSSVertex, emptyRange)
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun matchedRange ->
                   let newRange =
                       if buildSppf
                       then
                           let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(matchedRange.Range.InputRange, kvp.Key)
                           let rightSubRange =
                               let matchedRange =
                                  MatchedRange(
                                      matchedRange.Range.InputRange.StartPosition
                                      , matchedRange.Range.InputRange.EndPosition
                                      , currentDescriptor.RsmState
                                      , finalState)
                               let rangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                               MatchedRangeWithNode(matchedRange, rangeNode)                           
                           let leftSubRange = currentDescriptor.MatchedRange
                           matchedRanges.AddIntermediateNode(leftSubRange, rightSubRange)
                       else emptyRange 
                   Descriptor(finalState, matchedRange.Range.InputRange.EndPosition, currentDescriptor.GssVertex, newRange) |> addDescriptor)
        
        let inline handleTerminalEdge terminalSymbol (graphTargetVertex:IInputGraphVertex) (rsmTargetVertex:IRsmState) =
            let newMatchedRange =
                if buildSppf
                then
                    let currentlyCreatedNode = matchedRanges.AddTerminalNode(Range(currentDescriptor.InputPosition, graphTargetVertex), terminalSymbol)
                    let currentlyMatchedRange =
                        let matchedRange =
                            MatchedRange(
                                currentDescriptor.InputPosition
                                , graphTargetVertex
                                , currentDescriptor.RsmState
                                , rsmTargetVertex
                            )
                        let rangeNode = matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.TerminalNode currentlyCreatedNode)
                        MatchedRangeWithNode(matchedRange, rangeNode)                                            
                    matchedRanges.AddIntermediateNode (currentDescriptor.MatchedRange, currentlyMatchedRange)
                else emptyRange                
            Descriptor(rsmTargetVertex, graphTargetVertex, currentDescriptor.GssVertex, newMatchedRange) |> addDescriptor
        
        let handleEdge terminalSymbol finalVertex =
            let exists, finalSates = outgoingTerminalEdgesInRSM.TryGetValue terminalSymbol
            if exists && finalSates.Count > 0
            then
                    for state in finalSates do
                        handleTerminalEdge terminalSymbol finalVertex state                              
                            
        handleEdge EOF currentDescriptor.InputPosition
                                  
        for kvp in outgoingTerminalEdgesInGraph do
            for vertex in kvp.Value do
                handleEdge kvp.Key vertex            
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts reachableVertices
    | AllPaths -> QueryResult.MatchedRanges matchedRanges

let eval<'inputVertex when 'inputVertex: equality> (startVertices:HashSet<_>) (query:RSM) mode =
    let reachableVertices =
        let d = Dictionary<_,_>(startVertices.Count)
        startVertices
        |> Seq.iter (fun v -> d.Add(v, HashSet<_>()))
        d
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    evalFromState reachableVertices gss matchedRanges (startVertices:HashSet<_>) (query:RSM) mode