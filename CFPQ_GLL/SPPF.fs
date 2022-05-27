module CFPQ_GLL.SPPF

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core
open FSharpx.Collections

[<Measure>] type rsmRange
[<Measure>] type inputRange
[<Measure>] type rangeInfo
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

and [<Struct>] RangeInfo =
    val IntermediatePoints : HashSet<int64<rangeIntermediatePoint>>
    val Terminals : HashSet<int<terminalSymbol>>     
    val NonTerminals :  HashSet<int<rsmState>>
    val EpsilonNonTerminals :  HashSet<int<rsmState>>    
    
    new (intermediatePoints, terminals, nonTerminals, epsilonNonTerminals) =
        {
            IntermediatePoints = intermediatePoints
            Terminals = terminals
            NonTerminals = nonTerminals
            EpsilonNonTerminals = epsilonNonTerminals            
        }
      
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
    val RangeType: RangeType
    new (inputRange, rsmRange, rangeType) = {InputRange = inputRange; RSMRange = rsmRange; RangeType = rangeType}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeType) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo); RangeType = rangeType}

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)

let IS_TERMINAL = int64 (System.UInt64.MaxValue <<< 63)
let IS_NON_TERMINAL = int64 (System.UInt64.MaxValue <<< 63 >>> 1)
let IS_INTERMEDIATE = int64 (System.UInt64.MaxValue <<< 62)

let inline private packRange (rangeStart:int<'t>) (rangeEnd:int<'t>) =
    let _rangeStart = (int64 rangeStart) <<< 32
    let _rangeEnd = int64 rangeEnd
    (_rangeStart ||| _rangeEnd) |> LanguagePrimitives.Int64WithMeasure

let inline packIntermediatePoint (rsmIntermediatePoint:int<rsmState>) (inputIntermediatePoint:int<graphVertex>) : int64<rangeIntermediatePoint> =
    let _inputIntermediatePoint = (int64 inputIntermediatePoint) <<< BITS_FOR_RSM_STATE
    let _rsmIntermediatePoint = int64 rsmIntermediatePoint
    (_rsmIntermediatePoint ||| _inputIntermediatePoint) |> LanguagePrimitives.Int64WithMeasure

let inline unpackIntermediatePoint (inputIntermediatePoint : int64<rangeIntermediatePoint>) =
    let inputIntermediatePoint = int64 inputIntermediatePoint
    let _inputIntermediatePoint = int32 (inputIntermediatePoint &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure    
    let _rsmIntermediatePoint = int32 (inputIntermediatePoint &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    IntermediatePoint(_rsmIntermediatePoint, _inputIntermediatePoint)

let inline private unpackRange (range:int64<'t>) =
    let range = int64 range
    let _rangeStart = int32 (range >>> 32) |> LanguagePrimitives.Int32WithMeasure
    let _rangeEnd = int32 range |> LanguagePrimitives.Int32WithMeasure
    Range<'position>(_rangeStart, _rangeEnd)    

//|rangeType|rangeData|
// rangeType: two bits 
let inline packRangeInfo (range:MatchedRange) : int64<rangeInfo> =
    match range.RangeType with
    | RangeType.EpsilonNonTerminal n ->
        int64 n
    | RangeType.Terminal t ->
        (int64 t) ||| IS_TERMINAL
    | RangeType.NonTerminal n ->
        (int64 n) ||| IS_NON_TERMINAL
    | RangeType.Intermediate p ->
        (int64 p) ||| IS_INTERMEDIATE
    |> LanguagePrimitives.Int64WithMeasure

let inline unpackMatchedRange (rsmRange:int64<rsmRange>) (inputRange:int64<inputRange>) (rangeInfo:int64<rangeInfo>) =
    let rsmRange = unpackRange rsmRange
    let inputRange = unpackRange inputRange
    let rangeInfo = int64 rangeInfo
    let rangeType =
        if rangeInfo &&& IS_INTERMEDIATE = IS_INTERMEDIATE
        then (rangeInfo &&& ~~~IS_INTERMEDIATE) |> LanguagePrimitives.Int64WithMeasure |> RangeType.Intermediate
        elif rangeInfo &&& IS_TERMINAL = IS_TERMINAL
        then int32 (rangeInfo &&& ~~~IS_TERMINAL) |> LanguagePrimitives.Int32WithMeasure |> RangeType.Terminal
        elif rangeInfo &&& IS_NON_TERMINAL = IS_NON_TERMINAL
        then int32 (rangeInfo &&& ~~~IS_NON_TERMINAL) |> LanguagePrimitives.Int32WithMeasure |> RangeType.NonTerminal
        else int32 rangeInfo |> LanguagePrimitives.Int32WithMeasure |> RangeType.EpsilonNonTerminal     
    MatchedRange(inputRange, rsmRange, rangeType)
type MatchedRanges () =
    let ranges : SortedDictionary<int64<rsmRange>,SortedDictionary<int64<inputRange>,HashSet<int64<rangeInfo>>>> =
        SortedDictionary<_,_>()
    member this.AddMatchedRange (matchedRange: MatchedRange) =
        let rsmRange = packRange matchedRange.RSMRange.StartPosition matchedRange.RSMRange.EndPosition
        let inputRange = packRange matchedRange.InputRange.StartPosition matchedRange.InputRange.EndPosition
        let rangeInfo =
            let exists,dataForRSMRange = ranges.TryGetValue rsmRange
            if not exists
            then
                let newInputRangesDict = SortedDictionary<_,_>()
                let newRangeInfo = HashSet<_>()
                newInputRangesDict.Add(inputRange, newRangeInfo)
                ranges.Add(rsmRange, newInputRangesDict)
                newRangeInfo
            else
                let exists, dataForInputRange = dataForRSMRange.TryGetValue inputRange
                if not exists
                then
                    let newRangeInfo = HashSet<_>()
                    dataForRSMRange.Add(inputRange, newRangeInfo)
                    newRangeInfo
                else dataForInputRange
        
        packRangeInfo matchedRange |> rangeInfo.Add |> ignore      
                    
    member this.AddMatchedRange (leftSubRange: Option<MatchedRange>, rightSubRange: MatchedRange) =
        match leftSubRange with
        | None -> rightSubRange
        | Some leftSubRange ->
            let intermediatePoint = packIntermediatePoint leftSubRange.RSMRange.EndPosition leftSubRange.InputRange.EndPosition
            let newRange = MatchedRange(leftSubRange.InputRange.StartPosition
                                        , rightSubRange.InputRange.EndPosition
                                        , leftSubRange.RSMRange.StartPosition
                                        , rightSubRange.RSMRange.EndPosition
                                        , RangeType.Intermediate intermediatePoint)
            this.AddMatchedRange newRange
            newRange
     
    member this.Statistics() =
        ranges
        |> Seq.map (fun kvp -> kvp.Value.Count)
        |> Array.ofSeq
        |> Array.sortDescending
        ,
        [|for kvp in ranges do
              for kvp in kvp.Value do
                  kvp.Value.Count|]
        |> Array.sortDescending
    member this.ToSPPF(startV, query:RSM) =
        let rangeNodes = ResizeArray<RangeNode>()
        let isValidRange inputStart inputEnd rsmStart rsmEnd =
            let rsmRange = packRange rsmStart rsmEnd
            ranges.ContainsKey rsmRange && ranges.[rsmRange].ContainsKey(packRange inputStart inputEnd)
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
                
        for kvpRSM in ranges do
            for kvpInput in kvpRSM.Value do
                for rangeInfo in kvpInput.Value do
                    let range = unpackMatchedRange kvpRSM.Key kvpInput.Key rangeInfo
                    let rangeNode =
                        getRangeNode
                            range.InputRange.StartPosition
                            range.InputRange.EndPosition
                            range.RSMRange.StartPosition
                            range.RSMRange.EndPosition
                    match range.RangeType with
                    | RangeType.EpsilonNonTerminal n ->
                        EpsilonNode (range.InputRange.StartPosition, n)
                        |> NonRangeNode.EpsilonNode
                        
                    | RangeType.Terminal t ->
                        TerminalNode(t, range.InputRange.StartPosition, range.InputRange.EndPosition)
                        |> NonRangeNode.TerminalNode
                        
                    | RangeType.NonTerminal n ->
                        let rangeNodes =
                            [|
                                for final in query.GetFinalStatesForBoxWithThisStartState n do
                                    if isValidRange
                                           range.InputRange.StartPosition
                                           range.InputRange.EndPosition
                                           n
                                           final
                                    then
                                        yield
                                         getRangeNode
                                            range.InputRange.StartPosition
                                            range.InputRange.EndPosition
                                            n
                                            final
                            |]
                        NonTerminalNode(n, range.InputRange.StartPosition, range.InputRange.EndPosition,rangeNodes)
                        |> NonRangeNode.NonTerminalNode
                    | RangeType.Intermediate packedIntermediatePoint ->
                        let intermediatePoint = unpackIntermediatePoint packedIntermediatePoint                                                        
                        let leftNode =
                            getRangeNode
                                range.InputRange.StartPosition
                                intermediatePoint.InputPosition
                                range.RSMRange.StartPosition
                                intermediatePoint.RSMState
                        let rightNode =
                            getRangeNode
                                intermediatePoint.InputPosition
                                range.InputRange.EndPosition
                                intermediatePoint.RSMState
                                range.RSMRange.EndPosition                                
                        IntermediateNode(intermediatePoint.RSMState, intermediatePoint.InputPosition, leftNode, rightNode)
                        |> NonRangeNode.IntermediateNode
                    |> rangeNode.IntermediateNodes.Add                

        rangeNodes
        |> ResizeArray.filter (fun node -> node.RSMStartPosition = query.OriginalStartState
                                           && query.IsFinalStateForOriginalStartBox node.RSMEndPosition
                                           && startV |> Array.contains node.InputStartPosition)

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