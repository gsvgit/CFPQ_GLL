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

let run
        (reachableVertices:Dictionary<_,HashSet<_>>)
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (descriptorToProcess: Stack<Descriptor>)
        (startVertices:HashSet<IInputGraphVertex>)
        (query:RSM) mode =

    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true

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

    let dummyRangeNode = Unchecked.defaultof<RangeNode>

    let makeIntermediateNode (leftSubRange: MatchedRangeWithNode) (rightSubRange: MatchedRangeWithNode) =
        if buildSppf
        then matchedRanges.AddIntermediateNode (leftSubRange, rightSubRange)
        else
            let newMatchedRange = MatchedRange (leftSubRange.Range.InputRange.StartPosition
                        , rightSubRange.Range.InputRange.EndPosition
                        , leftSubRange.Range.RSMRange.StartPosition
                        , rightSubRange.Range.RSMRange.EndPosition)
            let rangeNode = dummyRangeNode
            MatchedRangeWithNode(newMatchedRange, rangeNode)

    let handleDescriptor (currentDescriptor:Descriptor) =

        gss.AddDescriptorToHandled currentDescriptor

        if currentDescriptor.RsmState.IsFinal
        then
            if (not buildSppf) && query.IsFinalStateForOriginalStartBox currentDescriptor.RsmState
            then
                let startPosition = currentDescriptor.GssVertex.InputPosition
                if startVertices.Contains startPosition
                then
                    reachableVertices.[startPosition].Add currentDescriptor.InputPosition |> ignore

            let matchedRange =
                match currentDescriptor.MatchedRange.Node with
                | None ->
                    let matchedRange = MatchedRange (
                             currentDescriptor.InputPosition
                             , currentDescriptor.InputPosition
                             , currentDescriptor.RsmState
                             , currentDescriptor.RsmState)

                    let newRangeNode =
                        if buildSppf
                        then
                            let currentlyCreatedNode = EpsilonNode (currentDescriptor.InputPosition, currentDescriptor.GssVertex.RsmState)
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.EpsilonNode currentlyCreatedNode)
                        else dummyRangeNode
                    MatchedRangeWithNode(matchedRange, newRangeNode)

                | _ -> currentDescriptor.MatchedRange

            gss.Pop(currentDescriptor, matchedRange)
            |> ResizeArray.iter (
                fun gssEdge ->
                    let newRange =
                        let leftSubRange = gssEdge.MatchedRange
                        let rightSubRange =
                            let matchedRange =
                              MatchedRange (
                                  currentDescriptor.GssVertex.InputPosition
                                  , currentDescriptor.InputPosition
                                  , match gssEdge.MatchedRange.Node with
                                    | None -> gssEdge.GssVertex.RsmState
                                    | _ -> gssEdge.MatchedRange.Range.RSMRange.EndPosition
                                  , gssEdge.RsmState)
                            let rangeNode =
                                if buildSppf
                                then
                                    let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(Range(currentDescriptor.GssVertex.InputPosition, currentDescriptor.InputPosition), currentDescriptor.GssVertex.RsmState)
                                    matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                                else dummyRangeNode
                            MatchedRangeWithNode(matchedRange, rangeNode)
                        makeIntermediateNode leftSubRange rightSubRange
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
                       let rightSubRange =
                           let newMatchedRange =
                              MatchedRange(
                                  matchedRange.Range.InputRange.StartPosition
                                  , matchedRange.Range.InputRange.EndPosition
                                  , currentDescriptor.RsmState
                                  , finalState)
                           let rangeNode =
                               if buildSppf
                               then
                                   let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(matchedRange.Range.InputRange, kvp.Key)
                                   matchedRanges.AddToMatchedRange(newMatchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                                else dummyRangeNode
                           MatchedRangeWithNode(newMatchedRange, rangeNode)
                       let leftSubRange = currentDescriptor.MatchedRange
                       makeIntermediateNode leftSubRange rightSubRange
                   Descriptor(finalState, matchedRange.Range.InputRange.EndPosition, currentDescriptor.GssVertex, newRange) |> addDescriptor)

        let inline handleTerminalOrEpsilonEdge terminalSymbol (graphTargetVertex:IInputGraphVertex) (rsmTargetVertex: IRsmState) =


            let newMatchedRange =
                let currentlyMatchedRange =
                    let matchedRange =
                        MatchedRange(
                            currentDescriptor.InputPosition
                            , graphTargetVertex
                            , currentDescriptor.RsmState
                            , rsmTargetVertex
                        )
                    let rangeNode =
                        if buildSppf
                        then
                            let currentlyCreatedNode = matchedRanges.AddTerminalNode(Range(currentDescriptor.InputPosition, graphTargetVertex), terminalSymbol)
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.TerminalNode currentlyCreatedNode)
                        else dummyRangeNode
                    MatchedRangeWithNode(matchedRange, rangeNode)
                makeIntermediateNode currentDescriptor.MatchedRange currentlyMatchedRange

            Descriptor(rsmTargetVertex, graphTargetVertex, currentDescriptor.GssVertex, newMatchedRange) |> addDescriptor

        let handleTerminalEdge terminalSymbol graphTargetVertex rsmTargetVertex =
            handleTerminalOrEpsilonEdge terminalSymbol graphTargetVertex rsmTargetVertex

        let handleEpsilonEdge targetVertex = handleTerminalOrEpsilonEdge Epsilon targetVertex currentDescriptor.RsmState

        let handleEdge terminalSymbol targetVertex =
            let exists, targetStates = outgoingTerminalEdgesInRSM.TryGetValue terminalSymbol
            if exists && targetStates.Count > 0
            then
                for state in targetStates do
                    handleTerminalEdge terminalSymbol targetVertex state

        handleEdge EOF currentDescriptor.InputPosition

        for kvp in outgoingTerminalEdgesInGraph do
            for vertex in kvp.Value do
                if kvp.Key = Epsilon then
                    handleEpsilonEdge vertex
                else
                    handleEdge kvp.Key vertex

    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts reachableVertices
    | AllPaths -> QueryResult.MatchedRanges matchedRanges

let evalFromState
        (reachableVertices:Dictionary<_,HashSet<_>>)
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (startVertices:HashSet<IInputGraphVertex>)
        (query:RSM) mode =

    let descriptorToProcess = Stack<_>()

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

    run
        reachableVertices
        gss
        matchedRanges
        descriptorToProcess
        startVertices
        query
        mode

let eval<'inputVertex when 'inputVertex: equality> (startVertices:HashSet<_>) (query:RSM) mode =
    let reachableVertices =
        let d = Dictionary<_,_>(startVertices.Count)
        startVertices
        |> Seq.iter (fun v -> d.Add(v, HashSet<_>()))
        d
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    evalFromState reachableVertices gss matchedRanges (startVertices:HashSet<_>) (query:RSM) mode

let onInputGraphChanged (changedVertices:seq<IInputGraphVertex>) =
    let descriptorsToContinueFrom = changedVertices |> Seq.collect (fun vertex -> vertex.Descriptors) |> Array.ofSeq
    descriptorsToContinueFrom
    |> Array.iter (fun descriptor ->
        let removed = descriptor.GssVertex.HandledDescriptors.Remove descriptor
        assert removed
        let removed = descriptor.InputPosition.Descriptors.Remove descriptor
        assert removed
        let removed = descriptor.RsmState.Descriptors.Remove descriptor
        assert removed
        )
    fun
        reachableVertices
        gss
        matchedRanges
        startVertices
        query
        mode
         ->
            run
                reachableVertices
                gss
                matchedRanges
                (Stack descriptorsToContinueFrom)
                startVertices
                query
                mode
