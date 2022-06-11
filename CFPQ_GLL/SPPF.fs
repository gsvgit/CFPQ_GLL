module CFPQ_GLL.SPPF

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core
open FSharpx.Collections

[<Measure>] type rangeIntermediatePoint
  
[<RequireQualifiedAccess>]
[<Struct>]
type RangeType =    
    | Terminal of terminal:int<terminalSymbol>
    | NonTerminal of nonTerminal:int<rsmState>
    | EpsilonNonTerminal of epsilonNonTerminal:int<rsmState>
    | Intermediate of intermediatePoint:int64<rangeIntermediatePoint>
    
[<Struct>]
type TerminalNode =
    val Terminal : int<terminalSymbol>
    val LeftPosition : int<graphVertex>
    val RightPosition : int<graphVertex>
    new (terminal, leftPosition, rightPosition)  =
        {
            Terminal = terminal
            LeftPosition = leftPosition
            RightPosition = rightPosition
        }

[<Struct>]
type EpsilonNode =    
    val Position : int<graphVertex>
    val NonTerminalStartState : int<rsmState>
    new (position, nonTerminalStartState)  =
        {            
            Position = position
            NonTerminalStartState = nonTerminalStartState
        }

type IntermediateNode (rsmState:int<rsmState>
                       , inputPosition:int<graphVertex>
                       , leftSubtree
                       , rightSubtree) =
    member this.RSMState = rsmState
    member this.InputPosition = inputPosition
    member this.LeftSubtree = leftSubtree
    member this.RightSubtree = rightSubtree

and RangeNode (inputStartPosition:int<graphVertex>
               , inputEndPosition:int<graphVertex>
               , rsmStartPosition:int<rsmState>
               , rsmEndPosition:int<rsmState>
               , intermediateNodes) =
    member this.InputStartPosition = inputStartPosition
    member this.InputEndPosition = inputEndPosition
    member this.RSMStartPosition = rsmStartPosition
    member this.RSMEndPosition = rsmEndPosition
    member this.IntermediateNodes = intermediateNodes    
  
and [<Struct>] IntermediatePoint =
    val RSMState : int<rsmState>
    val InputPosition : int<graphVertex>
    new (rsmState, inputPosition) = {RSMState = rsmState; InputPosition = inputPosition}

and [<Struct>] NonTerminalNode =
    val NonTerminalStartState : int<rsmState>
    val LeftPosition : int<graphVertex>
    val RightPosition : int<graphVertex>
    val RangeNodes : array<RangeNode>
    new (nonTerminalStartState, leftPosition, rightPosition, rangeNodes)  =
        {
            NonTerminalStartState = nonTerminalStartState
            LeftPosition = leftPosition
            RightPosition = rightPosition
            RangeNodes = rangeNodes
        }
                
and [<RequireQualifiedAccess>]NonRangeNode =
    | TerminalNode of TerminalNode
    | NonTerminalNode of NonTerminalNode
    | EpsilonNode of EpsilonNode
    | IntermediateNode of IntermediateNode
    
and [<Struct>] Range<[<Measure>]'position> =
    val StartPosition: int<'position>
    val EndPosition: int<'position>
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

[<Struct>]
type MatchedRange =
    val InputRange : Range<graphVertex>
    val RSMRange : Range<rsmState>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

[<Struct>]
type MatchedRangeWithType =
    val Range : MatchedRange
    val RangeType: RangeType
    new (range, rangeType) = {Range = range; RangeType = rangeType}
    new (inputRange, rsmRange, rangeType) = {Range = MatchedRange(inputRange, rsmRange); RangeType = rangeType}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeType) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); RangeType = rangeType}

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)

let inline packIntermediatePoint (rsmIntermediatePoint:int<rsmState>) (inputIntermediatePoint:int<graphVertex>) : int64<rangeIntermediatePoint> =
    let _inputIntermediatePoint = (int64 inputIntermediatePoint) <<< BITS_FOR_RSM_STATE
    let _rsmIntermediatePoint = int64 rsmIntermediatePoint
    (_rsmIntermediatePoint ||| _inputIntermediatePoint) |> LanguagePrimitives.Int64WithMeasure

let inline unpackIntermediatePoint (inputIntermediatePoint : int64<rangeIntermediatePoint>) =
    let inputIntermediatePoint = int64 inputIntermediatePoint
    let _inputIntermediatePoint = int32 (inputIntermediatePoint &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure    
    let _rsmIntermediatePoint = int32 (inputIntermediatePoint &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    IntermediatePoint(_rsmIntermediatePoint, _inputIntermediatePoint) 

type MatchedRanges (query:RSM) =
    let ranges : ResizeArray<HashSet<MatchedRangeWithType>> = ResizeArray<_>()
    let blockSize = 10000
    
    member this.AddMatchedRange (matchedRange: MatchedRangeWithType) =        
        let blockId = matchedRange.Range.InputRange.StartPosition / blockSize |> int
        if blockId >= ranges.Count
        then ranges.AddRange(Array.init (blockId - ranges.Count + 1) (fun _ -> HashSet<_>()))
        ranges.[blockId].Add matchedRange |> ignore
                    
    member this.AddMatchedRange (leftSubRange: Option<MatchedRangeWithType>, rightSubRange: MatchedRangeWithType) =
        match leftSubRange with
        | None -> rightSubRange
        | Some leftSubRange ->
            let intermediatePoint = packIntermediatePoint
                                        leftSubRange.Range.RSMRange.EndPosition
                                        leftSubRange.Range.InputRange.EndPosition
            let newRange = MatchedRangeWithType(leftSubRange.Range.InputRange.StartPosition
                                        , rightSubRange.Range.InputRange.EndPosition
                                        , leftSubRange.Range.RSMRange.StartPosition
                                        , rightSubRange.Range.RSMRange.EndPosition
                                        , RangeType.Intermediate intermediatePoint)
            this.AddMatchedRange newRange
            newRange
         
    member private this.Ranges with get () = ranges
    
    member this.UnionWith (newRanges:MatchedRanges) =
       if newRanges.Ranges.Count > this.Ranges.Count
       then this.Ranges.AddRange(Array.init (newRanges.Ranges.Count - this.Ranges.Count) (fun _ -> HashSet<_>()))
       else newRanges.Ranges.AddRange(Array.init (this.Ranges.Count - newRanges.Ranges.Count) (fun _ -> HashSet<_>()))
       ResizeArray.iter2
           (fun (_this:HashSet<_>) (_new:HashSet<_>) -> _this.UnionWith _new)
           this.Ranges
           newRanges.Ranges
    
    member this.GetShortestDistances (startVertices, finalVertices) =
        let rangesToTypes = Dictionary<MatchedRange,ResizeArray<RangeType>>()
        for block in ranges do
            for range in block do
                let exists, types = rangesToTypes.TryGetValue range.Range
                if exists
                then types.Add range.RangeType
                else rangesToTypes.Add(range.Range,ResizeArray[|range.RangeType|])
                
        let computedShortestDistances = Dictionary<MatchedRange,int>() 
        let rec computeShortestDistance range =
            let exists, computedDistance = computedShortestDistances.TryGetValue range
            if exists
            then computedDistance
            else
                let exists,types = rangesToTypes.TryGetValue range
                if not exists
                then System.Int32.MaxValue
                else 
                let distance =
                    [|
                        for rangeType in types ->
                            match rangeType with
                            | RangeType.EpsilonNonTerminal _ -> 0
                            | RangeType.Terminal _ -> 1
                            | RangeType.NonTerminal n ->
                                let finalStates = query.GetFinalStatesForBoxWithThisStartState n
                                [|
                                    for finalState in finalStates ->
                                        MatchedRange (range.InputRange.StartPosition,
                                                      range.InputRange.EndPosition,
                                                      n,
                                                      finalState)
                                        |> computeShortestDistance
                                |]
                                |> Array.min
                            | RangeType.Intermediate pos -> 
                                let pos = unpackIntermediatePoint pos
                                let leftRangeDistance =
                                    MatchedRange (range.InputRange.StartPosition,
                                                  pos.InputPosition,
                                                  range.RSMRange.StartPosition,
                                                  pos.RSMState)
                                    |> computeShortestDistance
                                let rightRangeDistance =
                                    MatchedRange (pos.InputPosition,
                                                  range.InputRange.EndPosition,
                                                  pos.RSMState,
                                                  range.RSMRange.EndPosition)
                                    |> computeShortestDistance
                                leftRangeDistance + rightRangeDistance
                    |]
                    |> Array.min
                computedShortestDistances.Add(range, distance)
                distance
        let res = ResizeArray<_>()
        for startVertex in startVertices do
            for finalVertex in finalVertices do
                for finalState in query.OriginalFinalStates do
                    MatchedRange(startVertex, finalVertex, query.StartState, finalState)
                    |> computeShortestDistance
                    |> fun distance -> res.Add (startVertex, finalVertex, distance)
        res
        
    member this.ToSPPF startVertices =
        let rangeNodes = ResizeArray<RangeNode>()
        
        let isValidRange inputStart inputEnd rsmStart rsmEnd =
            
            ranges.[int inputStart / blockSize]
            |> Seq.exists (fun range ->
                range.Range.InputRange.StartPosition = inputStart
                && range.Range.InputRange.EndPosition = inputEnd
                && range.Range.RSMRange.StartPosition = rsmStart
                && range.Range.RSMRange.EndPosition = rsmEnd)

        let getRangeNode inputStart inputEnd rsmStart rsmEnd =            
            let n =
                rangeNodes
                |> ResizeArray.tryFind (fun node -> node.InputStartPosition = inputStart
                                                    && node.InputEndPosition = inputEnd
                                                    && node.RSMStartPosition = rsmStart
                                                    && node.RSMEndPosition = rsmEnd)
            match n with
            | Some n -> n
            | None ->
                let newRangeNode = RangeNode(inputStart, inputEnd, rsmStart, rsmEnd, ResizeArray<_>())
                rangeNodes.Add newRangeNode
                newRangeNode
                
        for block in ranges do
            for range in block do                                    
                let rangeNode =
                    getRangeNode
                        range.Range.InputRange.StartPosition
                        range.Range.InputRange.EndPosition
                        range.Range.RSMRange.StartPosition
                        range.Range.RSMRange.EndPosition
                match range.RangeType with
                | RangeType.EpsilonNonTerminal n ->
                    EpsilonNode (range.Range.InputRange.StartPosition, n)
                    |> NonRangeNode.EpsilonNode
                    
                | RangeType.Terminal t ->
                    TerminalNode(t, range.Range.InputRange.StartPosition, range.Range.InputRange.EndPosition)
                    |> NonRangeNode.TerminalNode
                    
                | RangeType.NonTerminal n ->
                    let rangeNodes =
                        [|
                            for final in query.GetFinalStatesForBoxWithThisStartState n do
                                if isValidRange
                                       range.Range.InputRange.StartPosition
                                       range.Range.InputRange.EndPosition
                                       n
                                       final
                                then
                                    yield
                                     getRangeNode
                                        range.Range.InputRange.StartPosition
                                        range.Range.InputRange.EndPosition
                                        n
                                        final
                        |]
                    NonTerminalNode(n,
                                    range.Range.InputRange.StartPosition,
                                    range.Range.InputRange.EndPosition,
                                    rangeNodes)
                    |> NonRangeNode.NonTerminalNode
                | RangeType.Intermediate packedIntermediatePoint ->
                    let intermediatePoint = unpackIntermediatePoint packedIntermediatePoint                                                        
                    let leftNode =
                        getRangeNode
                            range.Range.InputRange.StartPosition
                            intermediatePoint.InputPosition
                            range.Range.RSMRange.StartPosition
                            intermediatePoint.RSMState
                    let rightNode =
                        getRangeNode
                            intermediatePoint.InputPosition
                            range.Range.InputRange.EndPosition
                            intermediatePoint.RSMState
                            range.Range.RSMRange.EndPosition                                
                    IntermediateNode(intermediatePoint.RSMState, intermediatePoint.InputPosition, leftNode, rightNode)
                    |> NonRangeNode.IntermediateNode
                |> rangeNode.IntermediateNodes.Add                

        rangeNodes
        |> ResizeArray.filter (fun node -> node.RSMStartPosition = query.OriginalStartState
                                           && query.IsFinalStateForOriginalStartBox node.RSMEndPosition
                                           && startVertices |> Array.contains node.InputStartPosition)

[<RequireQualifiedAccess>]
type TriplesStoredSPPFNode =
    | EpsilonNode of int<graphVertex> * int<rsmState>
    | TerminalNode of int<graphVertex> * int<terminalSymbol> * int<graphVertex>
    | NonTerminalNode of int<graphVertex> * int<rsmState> * int<graphVertex>
    | IntermediateNode of int<graphVertex> * int<rsmState>
    | RangeNode of int<graphVertex> * int<graphVertex> * int<rsmState> * int<rsmState> 

type TriplesStoredSPPF (sppf:ResizeArray<RangeNode>) =
    let mutable nodesCount = 0
    let nodes = Dictionary<_,_>()
    let edges = ResizeArray<_>()
    let visitedRangeNodes = Dictionary<_,_>()
    let addEdge parentId currentId =
        match parentId with
        | Some x -> edges.Add(x,currentId)
        | None -> ()
    
    let rec handleIntermediateNode parentId (node:IntermediateNode) =
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.IntermediateNode(node.InputPosition, node.RSMState))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
        handleRangeNode (Some currentId) node.LeftSubtree
        handleRangeNode (Some currentId) node.RightSubtree
    
    and handleTerminalNode parentId (node:TerminalNode) =
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.TerminalNode(node.LeftPosition, node.Terminal, node.RightPosition))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
        
    and handleEpsilonNode parentId (node:EpsilonNode) =
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.EpsilonNode(node.Position, node.NonTerminalStartState))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
    
    and handleNonTerminalNode parentId (node:NonTerminalNode) =
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.NonTerminalNode(node.LeftPosition, node.NonTerminalStartState, node.RightPosition))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
        node.RangeNodes
        |> Array.iter (handleRangeNode (Some currentId))
        
    and handleNonRangeNode parentId node =
        match node with
        | NonRangeNode.TerminalNode t -> handleTerminalNode parentId t
        | NonRangeNode.NonTerminalNode n -> handleNonTerminalNode parentId n
        | NonRangeNode.EpsilonNode e -> handleEpsilonNode parentId e
        | NonRangeNode.IntermediateNode p -> handleIntermediateNode parentId p
        
    and handleRangeNode parentId (node:RangeNode) =
        if visitedRangeNodes.ContainsKey node
        then addEdge parentId visitedRangeNodes.[node]
        else
            let currentId = nodesCount
            visitedRangeNodes.Add(node, currentId)
            nodes.Add(currentId, TriplesStoredSPPFNode.RangeNode(node.InputStartPosition, node.InputEndPosition, node.RSMStartPosition, node.RSMEndPosition))
            addEdge parentId currentId
            nodesCount <- nodesCount + 1
            node.IntermediateNodes
            |> ResizeArray.iter (handleNonRangeNode (Some currentId))
            
    do  sppf |> ResizeArray.iter (handleRangeNode None)
    
    let printEdge (x,y) = sprintf $"%i{x}->%i{y}"
    let printNode nodeId node =
        match node with
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to) ->
            sprintf $"%i{nodeId} [label = \"%i{_from}, %i{_terminal}, %i{_to}\", shape = rectangle]"
        | TriplesStoredSPPFNode.IntermediateNode (_inputPos, _rsmState) ->
            sprintf $"%i{nodeId} [label = \"Input: %i{_inputPos}; RSM: %i{_rsmState}\", shape = plain]"
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to) ->
            sprintf $"%i{nodeId} [label = \"%i{_from}, %i{_nonTerminal}, %i{_to}\", shape = invtrapezium]"
        | TriplesStoredSPPFNode.EpsilonNode (_pos, _nonTerminal) ->
            sprintf $"%i{nodeId} [label = \"Input: %i{_pos}; RSM: %i{_nonTerminal}\", shape = invhouse]"
        | TriplesStoredSPPFNode.RangeNode (_inputFrom, _inputTo, _rsmFrom, _rsmTo) ->
            sprintf $"%i{nodeId} [label = \"Input: %i{_inputFrom}, %i{_inputTo}; RSM: %i{_rsmFrom}, %i{_rsmTo}\", shape = ellipse]"
            
    member this.ToDot filePath =
        System.IO.File.WriteAllLines(filePath, ["digraph g {"; yield! (nodes |> Seq.map (fun kvp -> printNode kvp.Key kvp.Value)); yield! (ResizeArray.map printEdge edges); "}"])
    
    member this.Edges with get() = edges
    member this.Nodes with get () = nodes