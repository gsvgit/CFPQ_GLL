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
  
[<RequireQualifiedAccess>]
type RangeType =
    | CFG
    | Terminal of int<terminalSymbol>
    | NonTerminal of int<rsmState>
    | Intermediate
    
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
type IntermediateNode (rsmState, inputPosition, leftSubtree, rightSubtree) =
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
  

and [<Struct>] IntermediatePoint =
    val RSMState : int<rsmState>
    val InputPosition : int<graphVertex>
    new (rsmState, inputPosition) = {RSMState = rsmState; InputPosition = inputPosition}


and [<Struct>] RangeInfo =
    val IntermediatePoints : HashSet<int64<rangeIntermediatePoint>>
    val Terminals : HashSet<int<terminalSymbol>>     
    val NonTerminals :  HashSet<int<rsmState>>
    val IsCFG : ref<bool>
    
    new (intermediatePoints, terminals, nonTerminals, isCFG) =
        {
            IntermediatePoints = intermediatePoints
            Terminals = terminals
            IsCFG = isCFG
            NonTerminals = nonTerminals            
        }
      
and [<Struct>] NonTerminalNode =
    val NonTerminalStartState : int<rsmState>
    val LeftPosition : int<graphVertex>
    val RightPosition : int<graphVertex>
    //val PackedNodes : ResizeArray<PackedNode>
    new (nonTerminalStartState, leftPosition, rightPosition, packedNodes)  =
        {
            NonTerminalStartState = nonTerminalStartState
            LeftPosition = leftPosition
            RightPosition = rightPosition
            //PackedNodes = packedNodes
        }
                
and [<RequireQualifiedAccess>]NonRangeNode =
    | TerminalNode of TerminalNode
    | CFGEdgeNode of CFGEdgeNode
    | NonTerminalNode of NonTerminalNode
    | IntermediateNode of IntermediateNode
    
and [<Struct>] Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

[<Struct>]
type MatchedRange =
    val InputRange : Range<int<graphVertex>>
    val RSMRange : Range<int<rsmState>>
    val RangeType: RangeType
    new (inputRange, rsmRange, rangeType) = {InputRange = inputRange; RSMRange = rsmRange; RangeType = rangeType}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeType) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo); RangeType = rangeType}

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
    let ranges : Dictionary<int64<rsmRange>,Dictionary<int64<inputRange>,RangeInfo>> =
        Dictionary<_,_>()
    member this.AddMatchedRange (matchedRange: MatchedRange) =
        let rsmRange = packRange matchedRange.RSMRange.StartPosition matchedRange.RSMRange.EndPosition
        let inputRange = packRange matchedRange.InputRange.StartPosition matchedRange.InputRange.EndPosition
        let rangeInfo = 
            if not <| ranges.ContainsKey rsmRange
            then
                let newInputRangesDict = Dictionary<_,_>()
                let newRangeInfo = RangeInfo(HashSet<_>(),HashSet<_>(),HashSet<_>(),ref false)
                newInputRangesDict.Add(inputRange, newRangeInfo)
                ranges.Add(rsmRange,newInputRangesDict)
                newRangeInfo
            elif not <| ranges.[rsmRange].ContainsKey inputRange
            then
                let newRangeInfo = RangeInfo(HashSet<_>(),HashSet<_>(),HashSet<_>(),ref false)
                ranges.[rsmRange].Add(inputRange, newRangeInfo)
                newRangeInfo
            else ranges.[rsmRange].[inputRange]
        match matchedRange.RangeType with
        | RangeType.Terminal t   -> rangeInfo.Terminals.Add t |> ignore
        | RangeType.CFG -> rangeInfo.IsCFG.Value <- true
        | RangeType.NonTerminal n -> rangeInfo.NonTerminals.Add n |> ignore
        | RangeType.Intermediate -> ()
            
        rangeInfo
    member this.AddMatchedRange (leftSubRange: MatchedRange, rightSubRange: MatchedRange) =
        let newRange = MatchedRange(leftSubRange.InputRange.StartPosition
                                    , rightSubRange.InputRange.EndPosition
                                    , leftSubRange.RSMRange.StartPosition
                                    , rightSubRange.RSMRange.EndPosition
                                    , RangeType.Intermediate)
        let intermediatePoints = this.AddMatchedRange newRange
        let intermediatePoint = packIntermediatePoint leftSubRange.RSMRange.EndPosition leftSubRange.InputRange.EndPosition
        intermediatePoints.IntermediatePoints.Add intermediatePoint |> ignore
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
                let terminalNodes =
                    [
                        for terminal in kvp.Value.Terminals ->
                           NonRangeNode.TerminalNode <| TerminalNode (terminal,inputRange.StartPosition,inputRange.EndPosition) 
                    ]                                
                let intermediateNodes = 
                    [
                     for packedIntermediatePoint in kvp.Value.IntermediatePoints ->
                        let intermediatePoint = unpackIntermediatePoint packedIntermediatePoint                                                        
                        let leftNode = getRangeNode inputRange.StartPosition intermediatePoint.InputPosition rsmRange.StartPosition intermediatePoint.RSMState                                
                        let rightNode = getRangeNode intermediatePoint.InputPosition inputRange.EndPosition intermediatePoint.RSMState rsmRange.EndPosition
                        
                        NonRangeNode.IntermediateNode <| IntermediateNode(intermediatePoint.RSMState, intermediatePoint.InputPosition, leftNode, rightNode)
                    ]
                let rangeNode = getRangeNode inputRange.StartPosition inputRange.EndPosition rsmRange.StartPosition rsmRange.EndPosition
                
                if kvp.Value.IsCFG.Value
                then
                    CFGEdgeNode (inputRange.StartPosition,inputRange.EndPosition)
                    |> NonRangeNode.CFGEdgeNode
                    |> rangeNode.IntermediateNodes.Add
                
                rangeNode.IntermediateNodes.AddRange intermediateNodes
                rangeNode.IntermediateNodes.AddRange terminalNodes

        rangeNodes
                    
                