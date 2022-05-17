module CFPQ_GLL.SPPF

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core


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
    | Terminal of TerminalNode
    | NonTerminal of NonTerminalNode
    | Intermediate of IntermediateNode


[<Measure>] type rsmRange
[<Measure>] type inputRange
[<Measure>] type rangeIntermediatePoint

[<Struct>]
type Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) ={StartPosition = startPosition; EndPosition = endPosition}

[<Struct>]
type MatchedRange =
    val InputRange : Range<int<graphVertex>>
    val RSMRange : Range<int<rsmState>>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

let inline private packRange (rangeStart:int<'t>) (rangeEnd:int<'t>) =
    let _targetGSSVertex = (int64 rangeStart) <<< 32
    let _rsmState = int64 rangeEnd
    (_targetGSSVertex ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure

let inline packIntermediatePoint (rsmIntermediatePoint:int<rsmState>) (inputIntermediatePoint:int<graphVertex>) : int64<rangeIntermediatePoint> =
    let _rsmIntermediatePoint = (int64 rsmIntermediatePoint) <<< 32
    let _inputIntermediatePoint = int64 inputIntermediatePoint
    (_rsmIntermediatePoint ||| _inputIntermediatePoint) |> LanguagePrimitives.Int64WithMeasure


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