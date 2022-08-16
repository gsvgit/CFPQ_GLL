module CFPQ_GLL.Common

open System.Collections.Generic

[<Measure>] type terminalSymbol
[<Measure>] type distance

type Descriptor (rsmState: IRsmState, inputPosition: IInputGraphVertex, gssVertex: IGssVertex, matchedRange: MatchedRangeWithNode) =
    let hashCode =
        let mutable hash = 17
        hash <- hash * 23 + rsmState.GetHashCode()
        hash <- hash * 23 + inputPosition.GetHashCode()
        hash <- hash * 23 + gssVertex.GetHashCode()
        hash
    member this.RsmState = rsmState
    member this.InputPosition = inputPosition
    member this.GssVertex = gssVertex  
    member this.MatchedRange = matchedRange    
    override this.GetHashCode() = hashCode
    override this.Equals (y:obj) =
        y :? Descriptor
        && (y :?> Descriptor).RsmState = this.RsmState
        && (y :?> Descriptor).InputPosition = this.InputPosition
        && (y :?> Descriptor).GssVertex = this.GssVertex

and IRsmState =
    abstract OutgoingTerminalEdges : Dictionary<int<terminalSymbol>,HashSet<IRsmState>>
    abstract OutgoingNonTerminalEdges: Dictionary<IRsmState, HashSet<IRsmState>>
    abstract Descriptors: ResizeArray<Descriptor>
    abstract IsFinal: bool
    abstract IsStart: bool
    abstract Box: IRsmBox with get, set
    abstract NonTerminalNodes: ResizeArray<INonTerminalNode>  
    
and IRsmBox =
    abstract FinalStates: HashSet<IRsmState>
    
and IInputGraphVertex =
    abstract OutgoingEdges: Dictionary<int<terminalSymbol>, HashSet<IInputGraphVertex>>
    abstract Descriptors: HashSet<Descriptor>
    abstract TerminalNodes: Dictionary<IInputGraphVertex, Dictionary<int<terminalSymbol>, ITerminalNode>>
    abstract NonTerminalNodesStartedHere: Dictionary<IInputGraphVertex, Dictionary<IRsmState, INonTerminalNode>>
    //abstract NonTerminalNodesWithStartHere: HashSet<IInputGraphVertex * INonTerminalNode>
    abstract RangeNodes: Dictionary<MatchedRange, IRangeNode>
    abstract IntermediateNodes: Dictionary<MatchedRange, Dictionary<MatchedRange, IIntermediateNode>>

and [<Struct>] Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}
    
and [<Struct>] MatchedRange =
    val InputRange : Range<IInputGraphVertex>
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
    abstract LeftPosition : IInputGraphVertex
    abstract RightPosition : IInputGraphVertex
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
    abstract InputPosition: IInputGraphVertex
    abstract RsmState: IRsmState
    abstract OutgoingEdges: ResizeArray<IGssEdge> 
    abstract Popped: ResizeArray<MatchedRangeWithNode>
    abstract HandledDescriptors: HashSet<Descriptor>
    
and INonRangeNode =
    abstract Distance: int<distance>
    abstract Parents: HashSet<IRangeNode>
    
