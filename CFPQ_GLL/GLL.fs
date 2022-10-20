module CFPQ_GLL.GLL

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.SPPF
open CFPQ_GLL.DescriptorsStack
open FSharpx.Collections

type Mode =
    | ReachabilityOnly
    | AllPaths

[<RequireQualifiedAccess>]
type QueryResult =
    | ReachabilityFacts of Dictionary<ILinearInputGraphVertex,HashSet<ILinearInputGraphVertex>>
    | MatchedRanges of MatchedRanges

let private run
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (descriptorsToProcess: IDescriptorsStack)
        (startVertex:ILinearInputGraphVertex)
        (finalVertex:ILinearInputGraphVertex)
        (query:RSM) mode =

    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true

    let mutable findCorrect = false

    let inline addDescriptor (descriptor:Descriptor) =
        if not <| gss.IsThisDescriptorAlreadyHandled descriptor
        then
            descriptorsToProcess.Push descriptor

    let emptyRange =
        MatchedRangeWithNode
            (
                    Unchecked.defaultof<ILinearInputGraphVertex>
                  , Unchecked.defaultof<ILinearInputGraphVertex>
                  , Unchecked.defaultof<IRsmState>
                  , Unchecked.defaultof<IRsmState>
               )

    let dummyRangeNode = Unchecked.defaultof<RangeNode>

    let makeIntermediateNode (leftSubRange: MatchedRangeWithNode) (rightSubRange: MatchedRangeWithNode) =
        if buildSppf
        then matchedRanges.AddIntermediateNode (leftSubRange, rightSubRange, true)
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
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.EpsilonNode currentlyCreatedNode, true)
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
                                    findCorrect <- findCorrect ||
                                                   (startVertex = currentDescriptor.GssVertex.InputPosition)
                                                   && (finalVertex = currentDescriptor.InputPosition)
                                                   && (query.OriginalStartState = currentDescriptor.GssVertex.RsmState)

                                    matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode, true)
                                else dummyRangeNode
                            MatchedRangeWithNode(matchedRange, rangeNode)
                        makeIntermediateNode leftSubRange rightSubRange
                    Descriptor(gssEdge.RsmState, currentDescriptor.InputPosition, gssEdge.GssVertex, newRange, currentDescriptor.Weight)
                    |> addDescriptor
                )


        let outgoingTerminalEdgeInGraph = currentDescriptor.InputPosition.OutgoingEdge

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
               Descriptor(kvp.Key, currentDescriptor.InputPosition, newGSSVertex, emptyRange, currentDescriptor.Weight)
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun matchedRange ->
                   let weight,newRange =
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
                                   matchedRanges.AddToMatchedRange(newMatchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode, true)
                                else dummyRangeNode
                           MatchedRangeWithNode(newMatchedRange, rangeNode)
                       let leftSubRange = currentDescriptor.MatchedRange
                       let weight =
                           match rightSubRange.Node with
                           | Some x -> x.Distance
                           | None -> 0<distance>
                       weight, makeIntermediateNode leftSubRange rightSubRange

                   Descriptor(finalState, matchedRange.Range.InputRange.EndPosition, currentDescriptor.GssVertex, newRange, currentDescriptor.Weight + weight) |> addDescriptor)

        let inline handleTerminalOrEpsilonEdge terminalSymbol (graphEdgeTarget:TerminalEdgeTarget) (rsmTargetVertex: IRsmState) =

            let graphTargetVertex = graphEdgeTarget.TargetVertex

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
                            let currentlyCreatedNode =
                                 matchedRanges.AddTerminalNode(Range(currentDescriptor.InputPosition, graphTargetVertex), terminalSymbol,
                                       (graphEdgeTarget.Weight |> int |> LanguagePrimitives.Int32WithMeasure<_>))
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.TerminalNode currentlyCreatedNode, true)
                        else dummyRangeNode
                    MatchedRangeWithNode(matchedRange, rangeNode)
                makeIntermediateNode currentDescriptor.MatchedRange currentlyMatchedRange

            Descriptor(rsmTargetVertex, graphTargetVertex, currentDescriptor.GssVertex, newMatchedRange, currentDescriptor.Weight + (graphEdgeTarget.Weight |> int |> LanguagePrimitives.Int32WithMeasure<_>)) |> addDescriptor

        let handleTerminalEdge terminalSymbol graphTargetVertex rsmTargetVertex =
            handleTerminalOrEpsilonEdge terminalSymbol graphTargetVertex rsmTargetVertex

        let handleEpsilonEdge targetVertex = handleTerminalOrEpsilonEdge Epsilon targetVertex currentDescriptor.RsmState

        let handleEdge terminalSymbol targetVertex =
            let exists, targetStates = outgoingTerminalEdgesInRSM.TryGetValue terminalSymbol
            if exists && targetStates.Count > 0
            then
                for state in targetStates do
                    handleTerminalEdge terminalSymbol targetVertex state

        let errorRecoveryEdges =
            let errorRecoveryEdges = Dictionary()
            for terminal in currentDescriptor.RsmState.ErrorRecoveryLabels do
                errorRecoveryEdges.Add(terminal, TerminalEdgeTarget(currentDescriptor.InputPosition, 1<distance>))

            let _,targetVertex = currentDescriptor.InputPosition.OutgoingEdge
            errorRecoveryEdges.Add(Epsilon, TerminalEdgeTarget(targetVertex.TargetVertex, 1<distance>))
            errorRecoveryEdges

        let symbol, vertex = outgoingTerminalEdgeInGraph
        assert (symbol <> Epsilon)
        handleEdge symbol vertex

        for kvp in errorRecoveryEdges do
            if kvp.Key = Epsilon then
                handleEpsilonEdge kvp.Value
            else
                handleEdge kvp.Key kvp.Value

    let mutable cnt = 0
    while
        (true && ( (findCorrect && descriptorsToProcess.IsEmpty) |> not)) ||
        ((not true) && (descriptorsToProcess.IsEmpty |> not)) do
        let descriptor = descriptorsToProcess.Pop()
        cnt <- cnt + 1
        descriptor |> handleDescriptor

    printfn $"descriptors handled: %A{cnt}"

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts (Dictionary<_,_>())
    | AllPaths -> QueryResult.MatchedRanges matchedRanges

let evalFromState
        (descriptorToProcess:IDescriptorsStack)
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (startVertex:ILinearInputGraphVertex)
        (finalVertex:ILinearInputGraphVertex)
        (query:RSM) mode =

    let emptyRange =
        MatchedRangeWithNode
            (
                    Unchecked.defaultof<ILinearInputGraphVertex>
                  , Unchecked.defaultof<ILinearInputGraphVertex>
                  , Unchecked.defaultof<IRsmState>
                  , Unchecked.defaultof<IRsmState>
               )
    let gssVertex = gss.AddNewVertex(startVertex, query.StartState)
    Descriptor(query.StartState, startVertex, gssVertex, emptyRange, 0<distance>)
    |> descriptorToProcess.Push

    run
        gss
        matchedRanges
        descriptorToProcess
        startVertex
        finalVertex
        query
        mode


let errorRecoveringEval<'inputVertex when 'inputVertex: equality> finishVertices startVertices (query:RSM) mode =
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    evalFromState (ErrorRecoveringDescriptorsStack()) gss matchedRanges startVertices finishVertices (query:RSM) mode



//let onInputGraphChanged (changedVertices:seq<ILinearInputGraphVertex>) =
//    let descriptorsToContinueFrom = changedVertices |> Seq.collect (fun vertex -> vertex.Descriptors) |> Array.ofSeq
//    descriptorsToContinueFrom
//    |> Array.iter (fun descriptor ->
//        let removed = descriptor.GssVertex.HandledDescriptors.Remove descriptor
//        assert removed
//        let removed = descriptor.InputPosition.Descriptors.Remove descriptor
//        assert removed
//        let removed = descriptor.RsmState.Descriptors.Remove descriptor
//        assert removed
//        )
//    fun
//        reachableVertices
//        gss
//        matchedRanges
//        startVertices
//        query
//        mode
//         ->
//            run
//                gss
//                matchedRanges
//                (DefaultDescriptorsStack descriptorsToContinueFrom)
//                startVertices
//                (HashSet())
//                query
//                mode

