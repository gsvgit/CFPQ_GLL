module CFPQ_GLL.SPPF

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core
open FSharpx.Collections

[<Measure>] type rsmRange
[<Measure>] type inputRange
[<Measure>] type rangeIntermediatePoint

type IntermediateNode' (rsmState, inputPosition, leftSubtree, rightSubtree) =
    member this.RSMState = rsmState
    member this.InputPosition = inputPosition
    member this.LeftSubtree = leftSubtree
    member this.RightSubtree = rightSubtree

and RangeNode (inputStartPosition, inputEndPosition, rsmStartPosition, rsmEndPosition, intermediateNodes) =
    member this.InputStartPosition = inputStartPosition
    member this.InputEndPosition = inputEndPosition
    member this.RSMStartPosition = rsmStartPosition
    member this.RSMEndPosition = rsmEndPosition
    member this.IntermediateNodes = intermediateNodes    
  
[<Struct>]
type IntermediatePoint =
    val RSMState : int<rsmState>
    val InputPosition : int<graphVertex>
    new (rsmState, inputPosition) = {RSMState = rsmState; InputPosition = inputPosition}

[<Struct>]
type RangeInfo =
    val IntermediatePoints : HashSet<int64<rangeIntermediatePoint>>
    val Terminals : HashSet<int<terminalSymbol>>
    val IsCFG : bool 
    val NonTerminals :  HashSet<int<rsmState>>
    
    new (intermediatePoints, terminals, isCFG, nonTerminals) =
        {
            IntermediatePoints = intermediatePoints
            Terminals = terminals
            NonTerminals = nonTerminals
            IsCFG = isCFG
        }
    
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
type CFGEdgeNode =    
    val LeftPosition : int<graphVertex>
    val RightPosition : int<graphVertex>
    new (leftPosition, rightPosition)  =
        {            
            LeftPosition = leftPosition
            RightPosition = rightPosition
        }

[<Struct>]       
type  NonTerminalNode =
    val NonTerminalStartState : int<rsmState>
    val LeftPosition : int<graphVertex>
    val RightPosition : int<graphVertex>
    val PackedNodes : ResizeArray<PackedNode>
    new (nonTerminalStartState, leftPosition, rightPosition, packedNodes)  =
        {
            NonTerminalStartState = nonTerminalStartState
            LeftPosition = leftPosition
            RightPosition = rightPosition
            PackedNodes = packedNodes
        }
        
and [<Struct>] PackedNode =
    val CurrentRSMState : int<rsmState>
    val RightEndOfLeftChild : int<graphVertex>
    val LeftChild : Option<NonPackedNode>
    val RightChild : Option<NonPackedNode>
    new (currentRSMState, rightEndOfLeftChild, leftChild, rightChild) =
        {
            CurrentRSMState = currentRSMState
            RightEndOfLeftChild = rightEndOfLeftChild
            LeftChild = leftChild
            RightChild = rightChild
        }
        
and [<Struct>] IntermediateNode =
    val CurrentRSMState : int<rsmState>
    val LeftPosition : int<graphVertex>
    val RightPosition : int<graphVertex>
    val PackedNodes : ResizeArray<PackedNode>
    new (currentRSMState, leftPosition, rightPosition, packedNodes)  =
        {
            CurrentRSMState = currentRSMState
            LeftPosition = leftPosition
            RightPosition = rightPosition
            PackedNodes = packedNodes
        }
and [<RequireQualifiedAccess>]NonPackedNode =
    | TerminalNode of TerminalNode
    | CFGEdgeNode of CFGEdgeNode
    | NonTerminalNode of NonTerminalNode
    | IntermediateNode of IntermediateNode

type SPPF () =
    //let nonTerminalNodes =
    let terminalNodes = Dictionary<_,_> ()
    let cfgEdgeNodes = Dictionary<_,_> ()
    member this.GetTerminalNode (currentInputPosition, graphEdge:InputGraphTerminalEdge) =
        let packedTerminalNode = ()
        if terminalNodes.ContainsKey(packedTerminalNode)
        then terminalNodes.[packedTerminalNode]
        else
            let newNode = TerminalNode(graphEdge.TerminalSymbol, currentInputPosition, graphEdge.Vertex)
            terminalNodes.Add(packedTerminalNode, newNode)
            newNode
        
    member this.GetCFGEdgeNode (currentInputPosition, nextInputPosition) =
        let packedCFGEdgeNode = ()
        if cfgEdgeNodes.ContainsKey(packedCFGEdgeNode)
        then cfgEdgeNodes.[packedCFGEdgeNode]
        else
            let newNode = CFGEdgeNode(currentInputPosition, nextInputPosition)
            cfgEdgeNodes.Add(packedCFGEdgeNode, newNode)
            newNode
            
    member this.GetIntermediateNode () = ()
    
    member this.GetCFGEdgeNode () = ()



[<Struct>]
type Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

[<Struct>]
type MatchedRange =
    val InputRange : Range<int<graphVertex>>
    val RSMRange : Range<int<rsmState>>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)

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
    let _rsmIntermediatePoint = int32 (inputIntermediatePoint &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure    
    let _inputIntermediatePoint = int32 (inputIntermediatePoint &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    IntermediatePoint(_rsmIntermediatePoint, _inputIntermediatePoint)

let inline private unpackRange (range:int64<'t>) =
    let range = int64 range
    let _rangeStart = int32 (range >>> 32) |> LanguagePrimitives.Int32WithMeasure
    let _rangeEnd = int32 range |> LanguagePrimitives.Int32WithMeasure
    Range(_rangeStart, _rangeEnd)    
type MatchedRanges () =
    let ranges : Dictionary<int64<rsmRange>,Dictionary<int64<inputRange>,HashSet<int64<rangeIntermediatePoint>>>> =
        Dictionary<_,_>()
    member this.AddMatchedRange (matchedRange: MatchedRange) =
        let rsmRange = packRange matchedRange.RSMRange.StartPosition matchedRange.RSMRange.EndPosition
        let inputRange = packRange matchedRange.InputRange.StartPosition matchedRange.InputRange.EndPosition
        if not <| ranges.ContainsKey rsmRange
        then
            let newInputRangesDict = Dictionary<_,_>()
            let newIntermediatePoints = HashSet<_>()
            newInputRangesDict.Add(inputRange, newIntermediatePoints)
            ranges.Add(rsmRange,newInputRangesDict)
            newIntermediatePoints
        elif not <| ranges.[rsmRange].ContainsKey inputRange
        then
            let newIntermediatePoints = HashSet<_>()
            ranges.[rsmRange].Add(inputRange, newIntermediatePoints)
            newIntermediatePoints
        else ranges.[rsmRange].[inputRange]
    member this.AddMatchedRange (leftSubRange: MatchedRange, rightSubRange: MatchedRange) =
        let newRange = MatchedRange(leftSubRange.InputRange.StartPosition
                                    , rightSubRange.InputRange.EndPosition
                                    , leftSubRange.RSMRange.StartPosition
                                    , rightSubRange.RSMRange.EndPosition)
        let intermediatePoints = this.AddMatchedRange newRange
        let intermediatePoint = packIntermediatePoint leftSubRange.RSMRange.EndPosition leftSubRange.InputRange.EndPosition
        intermediatePoints.Add intermediatePoint |> ignore
        newRange
        
    member this.ToSPPF() =
        let rangeNodes = ResizeArray<RangeNode>()
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
                
        for kvp in ranges do
            let rsmRange = unpackRange kvp.Key
            for kvp in kvp.Value do
                let inputRange = unpackRange kvp.Key
                let intermediateNodes = 
                    [
                     for packedIntermediatePoint in kvp.Value ->
                        let intermediatePoint = unpackIntermediatePoint packedIntermediatePoint                                                        
                        let leftNode = getRangeNode inputRange.StartPosition intermediatePoint.InputPosition rsmRange.StartPosition intermediatePoint.RSMState                                
                        let rightNode = getRangeNode intermediatePoint.InputPosition inputRange.EndPosition intermediatePoint.RSMState rsmRange.EndPosition
                        
                        IntermediateNode'(intermediatePoint.RSMState, intermediatePoint.InputPosition, leftNode, rightNode)
                    ]
                let rangeNode = getRangeNode inputRange.StartPosition inputRange.EndPosition rsmRange.StartPosition rsmRange.EndPosition
                rangeNode.IntermediateNodes.AddRange intermediateNodes
                
        rangeNodes
                    
                