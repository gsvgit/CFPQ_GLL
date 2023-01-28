module CFPQ_GLL.Common

open System.Collections.Generic

[<Measure>] type terminalSymbol
[<Measure>] type distance


type Descriptor (rsmState: IRsmState, inputPosition: ILinearInputGraphVertex, gssVertex: IGssVertex, matchedRange: MatchedRangeWithNode, leftPartMinWeight: int<distance>) =
    let mutable leftPartMinWeight = leftPartMinWeight
    let hashCode =
        let mutable hash = 17
        hash <- hash * 23 + rsmState.GetHashCode()
        hash <- hash * 23 + inputPosition.GetHashCode()
        hash <- hash * 23 + gssVertex.GetHashCode()
        hash <- hash * 23 + int leftPartMinWeight
        //hash <- hash * 23 + matchedRange.GetHashCode()
        hash
    member val IsFinal = false with get, set
    member this.RsmState = rsmState
    member this.InputPosition = inputPosition
    member this.GssVertex = gssVertex
    member this.MatchedRange = matchedRange
    member this.Weight
        with get() = leftPartMinWeight
        and set v = leftPartMinWeight <- v
    override this.GetHashCode() = hashCode
    override this.Equals (y:obj) =
        y :? Descriptor
        && (y :?> Descriptor).RsmState = this.RsmState
        && (y :?> Descriptor).InputPosition = this.InputPosition
        && (y :?> Descriptor).GssVertex = this.GssVertex
        && (y :?> Descriptor).Weight = this.Weight
        //&& (y :?> Descriptor).MatchedRange = this.MatchedRange

and IRsmState =
    abstract OutgoingTerminalEdges : Dictionary<int<terminalSymbol>,HashSet<IRsmState>>
    abstract OutgoingNonTerminalEdges: Dictionary<IRsmState, HashSet<IRsmState>>
    abstract Descriptors: ResizeArray<Descriptor>
    abstract IsFinal: bool
    abstract IsStart: bool
    abstract Box: IRsmBox with get, set
    abstract NonTerminalNodes: ResizeArray<INonTerminalNode>
    abstract AddTerminalEdge: int<terminalSymbol>*IRsmState -> unit
    abstract AddNonTerminalEdge: IRsmState*IRsmState -> unit
    abstract ErrorRecoveryLabels: HashSet<int<terminalSymbol>>
and IRsmBox =
    abstract FinalStates: HashSet<IRsmState>

and ILinearInputGraphVertex =
    abstract OutgoingEdge: int<terminalSymbol> * TerminalEdgeTarget
    abstract Descriptors: HashSet<Descriptor>
    abstract TerminalNodes: Dictionary<ILinearInputGraphVertex, Dictionary<int<terminalSymbol>, ITerminalNode>>
    abstract NonTerminalNodesStartedHere: Dictionary<ILinearInputGraphVertex, Dictionary<IRsmState, INonTerminalNode>>
    //abstract NonTerminalNodesWithStartHere: HashSet<IInputGraphVertex * INonTerminalNode>
    abstract RangeNodes: Dictionary<MatchedRange, IRangeNode>
    abstract IntermediateNodes: Dictionary<MatchedRange, Dictionary<MatchedRange, IIntermediateNode>>
and [<Struct>] TerminalEdgeTarget =
    val TargetVertex: ILinearInputGraphVertex
    val Weight: int<distance>
    new (targetVertex, weight) = {TargetVertex = targetVertex; Weight = weight}
    new (targetVertex) = {TargetVertex = targetVertex; Weight = 0<distance>}

and [<Struct>] Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

and [<Struct>] MatchedRange =
    val InputRange : Range<ILinearInputGraphVertex>
    val RSMRange : Range<IRsmState>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

and [<Struct>] MatchedRangeWithNode =
    val Range : MatchedRange
    val Node: Option<IRangeNode>
    new (range, rangeNode) = {Range = range; Node = Some rangeNode}
    new (inputRange, rsmRange, rangeNode) = {Range = MatchedRange(inputRange, rsmRange); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeNode) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); Node = None}

and ITerminalNode =
    abstract Distance: int<distance> with get, set
    abstract Parents: HashSet<IRangeNode>
and IEpsilonNode =
    abstract Parents: HashSet<IRangeNode>
and INonTerminalNode =
    abstract Distance: int<distance> with get, set
    abstract Parents: HashSet<IRangeNode>
    abstract RangeNodes: ResizeArray<IRangeNode>
    abstract LeftPosition : ILinearInputGraphVertex
    abstract RightPosition : ILinearInputGraphVertex
    abstract NonTerminalStartState : IRsmState
and IIntermediateNode =
    abstract Distance: int<distance> with get, set
    abstract Parents: HashSet<IRangeNode>
and IRangeNode =
    abstract Distance: int<distance> with get, set
    abstract Parents: HashSet<INonRangeNode>
    abstract IntermediateNodes: HashSet<INonRangeNode>
and IGssEdge =
    abstract RsmState: IRsmState
    abstract GssVertex: IGssVertex
    abstract MatchedRange: MatchedRangeWithNode

and IGssVertex =
    abstract InputPosition: ILinearInputGraphVertex
    abstract RsmState: IRsmState
    abstract OutgoingEdges: ResizeArray<IGssEdge>
    abstract Popped: ResizeArray<MatchedRangeWithNode>
    abstract HandledDescriptors: HashSet<Descriptor>

and INonRangeNode =
    abstract Distance: int<distance>
    abstract Parents: HashSet<IRangeNode>

