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
type QueryResult =
    | ReachabilityFacts of Dictionary<LinearInputGraphVertexBase,HashSet<LinearInputGraphVertexBase>>
    | MatchedRanges of MatchedRanges

let logGll logLevel = Logging.printLog Logging.GLL logLevel


let private run
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (descriptorsToProcess: IDescriptorsStack)
        (startVertex:LinearInputGraphVertexBase)
        (finalVertex:LinearInputGraphVertexBase)
        (query:RSM) mode =

    // let encodeTerminal t =
    //     match int t with
    //     | -1 -> "epsilon"
    //     | 0 -> "["
    //     | 1 -> "space"
    //     | 2 -> "x"
    //     | 2147483646 -> "EOF"
    //     | _ -> failwith "Unknown terminal"
    //
    // let encodeNonTerminal nt =
    //     match nt with
    //     | 20852350 -> "Elem"
    //     | 17230008 -> "List"
    //     | _ -> failwith "Unknown nonterminal"

    let encodeTerminal t =
        match int t with
        | -1 -> "epsilon"
        | 0 -> "1"
        | 1 -> "space"
        | 2 -> "+"
        | 3 -> "r"
        | 4 -> ";"
        | 2147483646 -> "EOF"
        | _ -> failwith "Unknown terminal"

    let encodeNonTerminal nt =
        match nt with
        | 11315292 -> "Program"
        | 8361080 -> "Block"
        | 60620523 -> "IntExpr"
        | 55429698 -> "Statement"
        | 8713795 -> "Top lvl"
        | 6158855 -> "(10)"
        | 29105235 -> "(9)"
        | 62333418 -> "(3)"
        | 8140857 -> "(8)"
        | 24129853 -> "(4)"
        | 15842089 -> "(5)"
        | 61566768 -> "Start"
        | x -> $"Unknown: {x}"

    let buildSppf =
        match mode with
        | ReachabilityOnly -> false
        | AllPaths -> true

    let mutable findCorrect = false


    let inline addDescriptor (descriptor:Descriptor) =
        //assert ( not (gss.IsThisDescriptorAlreadyHandled descriptor && descriptor.IsFinal))
        if (not <| gss.IsThisDescriptorAlreadyHandled descriptor) || descriptor.IsFinal then
            descriptorsToProcess.Push descriptor

    let emptyRange =
        MatchedRangeWithNode
            (
                    Unchecked.defaultof<LinearInputGraphVertexBase>
                  , Unchecked.defaultof<LinearInputGraphVertexBase>
                  , Unchecked.defaultof<RsmState>
                  , Unchecked.defaultof<RsmState>
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
                                let currentlyCreatedNode = matchedRanges.AddNonTerminalNode(Range(currentDescriptor.GssVertex.InputPosition, currentDescriptor.InputPosition), currentDescriptor.GssVertex.RsmState)
                                findCorrect <-
                                               (startVertex = currentlyCreatedNode.LeftPosition)
                                               && (finalVertex = currentlyCreatedNode.RightPosition)
                                               && (query.OriginalStartState = currentlyCreatedNode.NonTerminalStartState)
                                ////logGll Logging.Trace $"findCorrect = {startVertex = currentlyCreatedNode.LeftPosition} && {finalVertex = currentlyCreatedNode.RightPosition} && {query.OriginalStartState = currentlyCreatedNode.NonTerminalStartState}"
                                matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                            else dummyRangeNode
                        MatchedRangeWithNode(matchedRange, rangeNode)
                    makeIntermediateNode leftSubRange rightSubRange
                let newWeight =
                    match newRange.Node with
                    | Some n -> n.Weight
                    | None -> 0<weight>
                let d = Descriptor(gssEdge.RsmState, currentDescriptor.InputPosition, gssEdge.GssVertex, newRange, newWeight)//currentDescriptor.Weight)
                //logGll Logging.Trace $"Adding descriptor {d.GetHashCode()} with rsm state {d.RsmState.GetHashCode() |> encodeNonTerminal} with distance {d.Weight}"
                d.IsFinal <- findCorrect
                addDescriptor d

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
               let d = Descriptor(kvp.Key, currentDescriptor.InputPosition, newGSSVertex, emptyRange, currentDescriptor.Weight)
               //logGll Logging.Trace $"Adding descriptor {d.GetHashCode()} with rsm state {d.RsmState.GetHashCode() |> encodeNonTerminal} with distance {d.Weight}"
               d |> addDescriptor
               for matchedRange in positionsForPops do               
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
                                   matchedRanges.AddToMatchedRange(newMatchedRange, NonRangeNode.NonTerminalNode currentlyCreatedNode)
                                else dummyRangeNode
                           MatchedRangeWithNode(newMatchedRange, rangeNode)
                       let leftSubRange = currentDescriptor.MatchedRange
                       let weight =
                           match rightSubRange.Node with
                           | Some x -> x.Weight
                           | None -> 0<weight>
                       weight, makeIntermediateNode leftSubRange rightSubRange
                   let d =  Descriptor(finalState, matchedRange.Range.InputRange.EndPosition, currentDescriptor.GssVertex, newRange, currentDescriptor.Weight + weight)
                   //logGll Logging.Trace $"Adding descriptor {d.GetHashCode()} with rsm state {d.RsmState.GetHashCode() |> encodeNonTerminal} with distance {d.Weight}"
                   d |> addDescriptor

        let handleTerminalOrEpsilonEdge terminalSymbol (graphEdgeTarget:TerminalEdgeTarget) (rsmTargetVertex: RsmState) =
            ////logGll Logging.Debug $"(handleTerminalOrEpsilonEdge) Terminal: {encodeTerminal terminalSymbol}  Weight: {graphEdgeTarget.Weight}"
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
                            matchedRanges.AddToMatchedRange(matchedRange, NonRangeNode.TerminalNode currentlyCreatedNode)
                        else dummyRangeNode
                    MatchedRangeWithNode(matchedRange, rangeNode)
                makeIntermediateNode currentDescriptor.MatchedRange currentlyMatchedRange

            let d = Descriptor(rsmTargetVertex, graphTargetVertex, currentDescriptor.GssVertex, newMatchedRange, currentDescriptor.Weight + (graphEdgeTarget.Weight |> int |> LanguagePrimitives.Int32WithMeasure<_>))
            //logGll Logging.Trace $"Adding descriptor {d.GetHashCode()} by terminal ( {encodeTerminal terminalSymbol} ) with rsm state {d.RsmState.GetHashCode() |> encodeNonTerminal} with distance {d.Weight}"
            d |> addDescriptor

        let handleTerminalEdge terminalSymbol graphTargetVertex rsmTargetVertex =
            handleTerminalOrEpsilonEdge terminalSymbol graphTargetVertex rsmTargetVertex

        let handleEpsilonEdge targetVertex =
            handleTerminalOrEpsilonEdge Epsilon targetVertex currentDescriptor.RsmState

        let handleEdge terminalSymbol targetVertex =
            let exists, targetStates = outgoingTerminalEdgesInRSM.TryGetValue terminalSymbol
            if exists && targetStates.Count > 0
            then
                for state in targetStates do
                    handleTerminalEdge terminalSymbol targetVertex state

        let errorRecoveryEdges =
            let errorRecoveryEdges = Dictionary()
            let currentTerminal,targetVertex = currentDescriptor.InputPosition.OutgoingEdge
            for terminal in currentDescriptor.RsmState.ErrorRecoveryLabels do
                if terminal <> currentTerminal
                then errorRecoveryEdges.Add(terminal, TerminalEdgeTarget(currentDescriptor.InputPosition, 1<weight>))
            errorRecoveryEdges.Add(Epsilon, TerminalEdgeTarget(targetVertex.TargetVertex, 1<weight>))
            errorRecoveryEdges

        for kvp in errorRecoveryEdges do
            if kvp.Key = Epsilon then
                handleEpsilonEdge kvp.Value
            else
                handleEdge kvp.Key kvp.Value
        
        let symbol, vertex = outgoingTerminalEdgeInGraph
        assert (symbol <> Epsilon)
        handleEdge symbol vertex



    let mutable weight = -1<weight>
    let mutable cnt = 0
    let mutable _continue = true
    while
        _continue do
        if cnt = 32
        then ()
        //logGll Logging.Trace $"------------ Step {cnt} ------------"
        let descriptor = descriptorsToProcess.Pop()
        match descriptor.MatchedRange.Node with
        | Some x -> ()
        | None -> ()
        //logGll Logging.Trace $"Processing descriptor {descriptor.GetHashCode()} "
        //logGll Logging.Trace $"Descriptor rsm state: {descriptor.RsmState.GetHashCode() |> encodeNonTerminal} with distance {descriptor.Weight}"
        //logGll Logging.Trace $"Descriptor graph edge: {fst descriptor.InputPosition.OutgoingEdge |> encodeTerminal}"
        //logGll Logging.Trace $"""Descriptor rsm terminals: {descriptor.RsmState.OutgoingTerminalEdges.Keys |> Seq.map encodeTerminal |> String.concat ", "}"""
        //logGll Logging.Trace $"""Descriptor rsm nonterminals: {descriptor.RsmState.OutgoingNonTerminalEdges.Keys |> Seq.map (fun x -> x.GetHashCode() |> encodeNonTerminal) |> String.concat ", " } """
        cnt <- cnt + 1
        if descriptor.Weight > weight then
            weight <- descriptor.Weight
            //printfn $"New weight: %A{weight}"
        if descriptor.IsFinal
        then _continue <- false
        else descriptor |> handleDescriptor

    //printfn $"Processed {cnt} descriptors"
    ////logGll Logging.Info $"descriptors handled: %A{cnt}"

    match mode with
    | ReachabilityOnly -> QueryResult.ReachabilityFacts (Dictionary<_,_>())
    | AllPaths -> QueryResult.MatchedRanges matchedRanges
    ,cnt

let evalFromState
        (descriptorToProcess:IDescriptorsStack)
        (gss:GSS)
        (matchedRanges:MatchedRanges)
        (startVertex:LinearInputGraphVertexBase)
        (finalVertex:LinearInputGraphVertexBase)
        (query:RSM) mode =

    let emptyRange =
        MatchedRangeWithNode
            (
                    Unchecked.defaultof<LinearInputGraphVertexBase>
                  , Unchecked.defaultof<LinearInputGraphVertexBase>
                  , Unchecked.defaultof<RsmState>
                  , Unchecked.defaultof<RsmState>
               )
    let gssVertex = gss.AddNewVertex(startVertex, query.StartState, 0<weight>)
    Descriptor(query.StartState, startVertex, gssVertex, emptyRange, 0<weight>)
    |> descriptorToProcess.Push

    run
        gss
        matchedRanges
        descriptorToProcess
        startVertex
        finalVertex
        query
        mode


let errorRecoveringEval<'inputVertex when 'inputVertex: equality> finishVertex startVertex (query:RSM) mode =
    let gss = GSS()
    let matchedRanges = MatchedRanges()
    evalFromState (ErrorRecoveringDescriptorsStack()) gss matchedRanges startVertex finishVertex (query:RSM) mode


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

