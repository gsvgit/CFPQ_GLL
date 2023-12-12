module CFPQ_GLL.Common

open System
open System.Collections.Generic

[<Measure>] type terminalSymbol
[<Measure>] type weight
[<Measure>] type rsmStateId
[<Measure>] type inputGraphVertex

type ITerminal<'token when 'token: equality> =
    abstract Token: 'token

type Char =
    Char of char | EOF | Epsilon 
    override this.ToString() =
        match this with
        | Char c -> string c
        | EOF -> "EOF"
        | Epsilon -> "ε"
type INonterminal =
    abstract Name: string
    
type NonterminalBase (name: string) =
    interface INonterminal with    
        member this.Name = name
    
let getFirstFreeRsmStateId =
    let mutable cnt = 0<rsmStateId>
    fun () ->
        let res = cnt
        cnt <- cnt + 1<rsmStateId>
        res
type Descriptor<'token when 'token: equality> (rsmState: IRsmState<'token>, inputPosition: IInputGraphVertex<'token>, gssVertex: IGssVertex<'token>, matchedRange: MatchedRangeWithNode<'token>) =    
    let hashCode =
        let mutable hash = 17
        hash <- hash * 23 + rsmState.GetHashCode()
        hash <- hash * 23 + inputPosition.GetHashCode()
        hash <- hash * 23 + gssVertex.GetHashCode()
        hash
    
    let mutable isAlive = true
    member val IsFinal = false with get, set
    member this.RsmState = rsmState
    member this.InputPosition = inputPosition
    member this.GssVertex = gssVertex
    member this.MatchedRange = matchedRange
    member this.IsAlive with get () = isAlive && this.MatchedRange.IsAlive()
                        and set v = isAlive <- v
     member this.Weight
        with get() =
            let treeWeight =
                match this.MatchedRange.Node with
                | Some n ->
                    let isAlive, n = n.TryGetTarget()
                    if isAlive && n.IsAlive
                    then n.Weight
                    else failwith "An attempt to get weight of invalid descriptor"
                | None -> 0<weight>
                
            this.GssVertex.MinimalWeightOfLeftPart + treeWeight                        
    override this.GetHashCode() = hashCode
    override this.Equals (y:obj) =
        y :? Descriptor<'token>
        && (y :?> Descriptor<'token>).RsmState = this.RsmState
        && (y :?> Descriptor<'token>).InputPosition = this.InputPosition
        && (y :?> Descriptor<'token>).GssVertex = this.GssVertex

and IRsmState<'token when 'token: equality> =
    abstract ErrorRecoveryLabels: HashSet<'token>
    abstract OutgoingTerminalEdges : Dictionary<'token, HashSet<IRsmState<'token>>>
    abstract OutgoingNonTerminalEdges: Dictionary<IRsmState<'token>, HashSet<IRsmState<'token>>>
    abstract Descriptors: ResizeArray<WeakReference<Descriptor<'token>>>
    abstract GetValidDescriptors: unit -> seq<Descriptor<'token>>
    abstract IsFinal: bool
    abstract IsStart: bool
    abstract Box: IRsmBox<'token> with get, set
    abstract NonTerminalNodes: ResizeArray<INonTerminalNode<'token>>  
    abstract AddTerminalEdge: 'token * IRsmState<'token> -> unit
    abstract AddNonTerminalEdge: IRsmState<'token> * IRsmState<'token> -> unit
    abstract Id: int<rsmStateId>
    
and IRsmBox<'token when 'token: equality> =
    abstract FinalStates: HashSet<IRsmState<'token>>
    abstract Nonterminal: INonterminal
     
and [<Struct>] TerminalEdgeTarget<'token when 'token: equality> =
    val TargetVertex: IInputGraphVertex<'token>
    val Weight: int<weight>
    new (targetVertex, weight) = {TargetVertex = targetVertex; Weight = weight}
    new (targetVertex) = {TargetVertex = targetVertex; Weight = 0<weight>}
    
and IInputGraphVertex<'token when 'token: equality> =
    abstract Id: int<inputGraphVertex>    
    abstract ForAllOutgoingEdges: Descriptor<'token> -> ('token -> TerminalEdgeTarget<'token> -> unit) -> (TerminalEdgeTarget<'token> -> unit) -> unit 
    abstract Descriptors: HashSet<WeakReference<Descriptor<'token>>>
    abstract GetValidDescriptors: unit -> seq<Descriptor<'token>>
    abstract TerminalNodes: Dictionary<IInputGraphVertex<'token>, Dictionary<'token, WeakReference<ITerminalNode<'token>>>>
    abstract NonTerminalNodesStartedHere: Dictionary<IInputGraphVertex<'token>, Dictionary<IRsmState<'token>, WeakReference<INonTerminalNode<'token>>>>    
    abstract RangeNodes: Dictionary<MatchedRange<'token>, WeakReference<IRangeNode<'token>>>
    abstract IntermediateNodes: Dictionary<MatchedRange<'token>, Dictionary<MatchedRange<'token>, WeakReference<IIntermediateNode<'token>>>>

and [<Struct>] Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

and [<Struct>] MatchedRange<'token when 'token: equality> =
    val InputRange : Range<IInputGraphVertex<'token>>
    val RSMRange : Range<IRsmState<'token>>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

and [<Struct>] MatchedRangeWithNode<'token when 'token: equality> =
    val Range : MatchedRange<'token>
    val Node: Option<WeakReference<IRangeNode<'token>>>
    member this.IsAlive() =
        match this.Node with
        | None -> true
        | Some n ->
            let valid,n = n.TryGetTarget()
            valid && n.IsAlive
    new (range, rangeNode) = {Range = range; Node = rangeNode |> WeakReference<IRangeNode<'token>> |> Some}
    new (inputRange, rsmRange, rangeNode) = {Range = MatchedRange<'token>(inputRange, rsmRange); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeNode) = {Range = MatchedRange<'token>(inputFrom, inputTo, rsmFrom, rsmTo); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {Range = MatchedRange<'token>(inputFrom, inputTo, rsmFrom, rsmTo); Node = None}
    
and ISppfNode<'ParentType when 'ParentType: not struct> =
    abstract IsAlive: bool with get, set
    abstract Parents: ResizeArray<WeakReference<'ParentType>>
    abstract GetValidParents: unit -> seq<'ParentType>
    
and ITerminalNode<'token when 'token: equality> =
    inherit ISppfNode<IRangeNode<'token>>
    abstract Weight: int<weight> with get, set
and IEpsilonNode<'token when 'token: equality> =    
    inherit ISppfNode<IRangeNode<'token>>
and INonTerminalNode<'token when 'token: equality> =
    inherit ISppfNode<IRangeNode<'token>>
    abstract Weight: int<weight> with get, set    
    abstract RangeNodes: ResizeArray<IRangeNode<'token>>
    abstract LeftPosition : IInputGraphVertex<'token>
    abstract RightPosition : IInputGraphVertex<'token>
    abstract NonTerminalStartState : IRsmState<'token>
and IIntermediateNode<'token when 'token: equality> =
    inherit ISppfNode<IRangeNode<'token>>    
    abstract Weight: int<weight> with get, set
and IRangeNode<'token when 'token: equality> =
    inherit ISppfNode<INonRangeNode<'token>>    
    abstract Weight: int<weight> with get, set
    abstract IntermediateNodes: HashSet<INonRangeNode<'token>>
    abstract GssEdges: ResizeArray<IGssVertex<'token> * IGssEdge<'token>>
    
and IGssEdge<'token when 'token: equality> =
    abstract RsmState: IRsmState<'token>
    abstract GssVertex: IGssVertex<'token>
    abstract MatchedRange: MatchedRangeWithNode<'token>

and IGssVertex<'token when 'token: equality> =
    abstract MinimalWeightOfLeftPart: int<weight> with get, set
    abstract InputPosition: IInputGraphVertex<'token>
    abstract RsmState: IRsmState<'token>
    abstract OutgoingEdges: ResizeArray<IGssEdge<'token>>
    abstract Popped: ResizeArray<MatchedRangeWithNode<'token>>
    abstract HandledDescriptors: HashSet<Descriptor<'token>>

and INonRangeNode<'token when 'token: equality> =
    inherit ISppfNode<IRangeNode<'token>>
    abstract Weight: int<weight>    


let tryGetPossiblyWeakSppfNode (dict:Dictionary<'key,WeakReference<#ISppfNode<'t>>>) (key:'key) =
    let exists, value = dict.TryGetValue key
    let isAlive, value =
        if exists
        then value.TryGetTarget()
        else
            let removed = dict.Remove key
            assert removed
            false, Unchecked.defaultof<_>
    if isAlive && value.IsAlive
    then true, value
    else
        let removed = dict.Remove key
        assert removed
        false, value
