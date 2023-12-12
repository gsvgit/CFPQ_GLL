module CFPQ_GLL.GLL

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.SPPF
open CFPQ_GLL.DescriptorsStack

type Mode =
    | ReachabilityOnly
    | AllPaths

[<RequireQualifiedAccess>]
type QueryResult<'token when 'token: equality> =
    | ReachabilityFacts of Dictionary<IInputGraphVertex<'token>,HashSet<IInputGraphVertex<'token>>>
    | MatchedRanges of MatchedRanges<'token>


let logGll logLevel = Logging.printLog Logging.GLL logLevel
let logDescriptorInfo (descriptor: Descriptor<'token>) =
    let ppSpaces = Logging.formatAsLog Logging.GLL "" |> String.length |> fun x -> String.replicate x " "
    let msg =
        [
            $"\n{ppSpaces}Processing descriptor {descriptor.GetHashCode()}"
            $"Descriptor rsm state: {descriptor.RsmState.Box.Nonterminal.Name} with distance {descriptor.Weight}"
            //$"Descriptor graph edge: {fst descriptor.InputPosition.OutgoingEdges}"
            $"""Descriptor rsm terminals: {descriptor.RsmState.OutgoingTerminalEdges.Keys |> Seq.map string |> String.concat ", "}"""
            $"""Descriptor rsm nonterminals: {descriptor.RsmState.OutgoingNonTerminalEdges.Keys |> Seq.map (fun x -> x.Box.Nonterminal.Name) |> String.concat ", " } """
            $"Weight: %A{descriptor.Weight}"
            $"Descriptor is final: {descriptor.IsFinal}"
        ] |> String.concat $"\n{ppSpaces}"
    logGll Logging.Trace $"{msg}"

let logDescriptorCreated (d: Descriptor<'token>) =
    logGll Logging.Trace $"Adding descriptor {d.GetHashCode()} with rsm state {d.RsmState.Box.Nonterminal.Name} with distance {d.Weight}"

let logDescriptorCreatedByTerminal t (d: Descriptor<'token>) =
    logGll Logging.Trace $"Adding descriptor {d.GetHashCode()} by terminal ( {t} ) with rsm state {d.RsmState.Box.Nonterminal.Name} with distance {d.Weight}"


let private run
        (gss:GSS<'token>)
        (matchedRanges:MatchedRanges<'token>)
        (descriptorsToProcess: IDescriptorsStack<'token>)
        (startVertex:IInputGraphVertex<'token>)
        (finalVertex:IInputGraphVertex<'token>)
        (query:RSM<'token>) (epsilon: 'token) mode =

    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true

    let mutable findCorrect = false


    let inline addDescriptor (descriptor:Descriptor<'token>) =
        if (not <| gss.IsThisDescriptorAlreadyHandled descriptor) || descriptor.IsFinal then
            descriptorsToProcess.Push descriptor

    let emptyRange =
        MatchedRangeWithNode
            (
                    Unchecked.defaultof<IInputGraphVertex<'token>>
                  , Unchecked.defaultof<IInputGraphVertex<'token>>
                  , Unchecked.defaultof<RsmState<'token>>
                  , Unchecked.defaultof<RsmState<'token>>
               )

    let dummyRangeNode = Unchecked.defaultof<IRangeNode<_>>

    let makeIntermediateNode (leftSubRange: MatchedRangeWithNode<'token>) (rightSubRange: MatchedRangeWithNode<'token>) =
        if buildSppf
        then matchedRanges.AddIntermediateNode (leftSubRange, rightSubRange)
        else
            let newMatchedRange = MatchedRange (leftSubRange.Range.InputRange.StartPosition
                        , rightSubRange.Range.InputRange.EndPosition
                        , leftSubRange.Range.RSMRange.StartPosition
                        , rightSubRange.Range.RSMRange.EndPosition)
            let rangeNode = dummyRangeNode
            MatchedRangeWithNode(newMatchedRange, rangeNode)

    let handleDescriptor (currentDescriptor:Descriptor<'token>) =

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
                            
                            let currentlyCreatedNode = matchedRanges.CreateEpsilonNode (currentDescriptor.InputPosition, currentDescriptor.GssVertex.RsmState)
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.EpsilonNode currentlyCreatedNode)
                        else dummyRangeNode
                    MatchedRangeWithNode(matchedRange, newRangeNode)

                | _ -> currentDescriptor.MatchedRange

            for gssEdge in gss.Pop(currentDescriptor, matchedRange)do
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
                                let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(Range<_>(currentDescriptor.GssVertex.InputPosition, currentDescriptor.InputPosition), currentDescriptor.GssVertex.RsmState)
                                findCorrect <-
                                               (startVertex = currentlyCreatedNode.LeftPosition)
                                               && (finalVertex = currentlyCreatedNode.RightPosition)
                                               && (query.OriginalStartState = currentlyCreatedNode.NonTerminalStartState)
                                matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                            else dummyRangeNode
                        MatchedRangeWithNode(matchedRange, rangeNode)
                    makeIntermediateNode leftSubRange rightSubRange
                let d = Descriptor(gssEdge.RsmState, currentDescriptor.InputPosition, gssEdge.GssVertex, newRange)
                d.IsFinal <- findCorrect
                logDescriptorCreated d
                addDescriptor d

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
               let d = Descriptor(kvp.Key, currentDescriptor.InputPosition, newGSSVertex, emptyRange)
               logDescriptorCreated d
               addDescriptor d
               for matchedRange in positionsForPops do
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
                   let d = Descriptor(finalState, matchedRange.Range.InputRange.EndPosition, currentDescriptor.GssVertex, newRange)
                   logDescriptorCreated d
                   addDescriptor d

        let handleTerminalOrEpsilonEdge terminalSymbol (graphEdgeTarget:TerminalEdgeTarget<'token>) (rsmTargetVertex: IRsmState<'token>) =
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
                                 matchedRanges.AddTerminalNode(Range<_>(currentDescriptor.InputPosition, graphTargetVertex), terminalSymbol,
                                       (graphEdgeTarget.Weight |> int |> LanguagePrimitives.Int32WithMeasure<_>))
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.TerminalNode currentlyCreatedNode)
                        else dummyRangeNode
                    MatchedRangeWithNode(matchedRange, rangeNode)
                makeIntermediateNode currentDescriptor.MatchedRange currentlyMatchedRange
            let d = Descriptor(rsmTargetVertex, graphTargetVertex, currentDescriptor.GssVertex, newMatchedRange)
            logDescriptorCreatedByTerminal terminalSymbol d
            addDescriptor d

        let handleTerminalEdge terminalSymbol graphTargetVertex rsmTargetVertex =
            handleTerminalOrEpsilonEdge terminalSymbol graphTargetVertex rsmTargetVertex

        let handleEpsilonEdge targetVertex =
            handleTerminalOrEpsilonEdge epsilon targetVertex currentDescriptor.RsmState

        let handleEdge terminalSymbol targetVertex =
            let exists, targetStates = outgoingTerminalEdgesInRSM.TryGetValue terminalSymbol
            if exists && targetStates.Count > 0
            then
                for state in targetStates do
                    handleTerminalEdge terminalSymbol targetVertex state

        currentDescriptor.InputPosition.ForAllOutgoingEdges currentDescriptor handleEdge handleEpsilonEdge
        
        

    let mutable cnt = 0
    let mutable _continue = true
    while _continue do
        let descriptor = descriptorsToProcess.Pop()
        logDescriptorInfo descriptor
        cnt <- cnt + 1
        if descriptor.IsFinal
        then _continue <- false
        else descriptor |> handleDescriptor

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts (Dictionary<_,_>())
    | AllPaths -> QueryResult.MatchedRanges matchedRanges
    ,cnt

let evalFromState
        (descriptorToProcess:IDescriptorsStack<'token>)
        (gss:GSS<'token>)
        (matchedRanges:MatchedRanges<'token>)
        (startVertex:IInputGraphVertex<'token>)
        (finalVertex:IInputGraphVertex<'token>)
        (query:RSM<'token>) mode =

    let emptyRange =
        MatchedRangeWithNode
            (
                    Unchecked.defaultof<IInputGraphVertex<'token>>
                  , Unchecked.defaultof<IInputGraphVertex<'token>>
                  , Unchecked.defaultof<RsmState<'token>>
                  , Unchecked.defaultof<RsmState<'token>>
               )
    let gssVertex = gss.AddNewVertex(startVertex, query.StartState, 0<weight>)
    Descriptor(query.StartState, startVertex, gssVertex, emptyRange)
    |> descriptorToProcess.Push

    run
        gss
        matchedRanges
        descriptorToProcess
        startVertex
        finalVertex
        query
        mode


let errorRecoveringEval<'inputVertex, 'token when 'inputVertex: equality and 'token: equality> finishVertex startVertex (query:RSM<'token>) mode (epsilon: 'token) =
    let gss = GSS()
    let matchedRanges = MatchedRanges(epsilon)
    evalFromState (ErrorRecoveringDescriptorsStack()) gss matchedRanges startVertex finishVertex (query:RSM<'token>) mode

let onInputGraphChanged (changedVertices:seq<IInputGraphVertex<'token>>) =

    let descriptorsToContinueFrom = changedVertices |> Seq.collect (fun vertex -> vertex.GetValidDescriptors()) |> Array.ofSeq
    descriptorsToContinueFrom
    |> Array.iter (fun descriptor ->
        //descriptor.IsAlive <- false
        let removed = descriptor.GssVertex.HandledDescriptors.Remove descriptor
        assert removed
        //let removed = descriptor.InputPosition.Descriptors.Remove descriptor
        //assert removed
        //let removed = descriptor.RsmState.Descriptors.Remove descriptor
        //assert removed
        )
    fun
        //reachableVertices
        gss
        matchedRanges
        startVertices
        query
        mode
         ->
            run
                //reachableVertices
                gss
                matchedRanges
                (DefaultDescriptorsStack descriptorsToContinueFrom)
                startVertices
                query
                mode
