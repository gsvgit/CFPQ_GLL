module CFPQ_GLL.Common

open System.Collections.Generic
open System.Security.Cryptography

[<Measure>] type terminalSymbol
[<Measure>] type weight
[<Measure>] type rsmStateId

[<Measure>] type inputGraphVertex

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
type Descriptor (rsmState: RsmState, inputPosition: LinearInputGraphVertexBase, gssVertex: IGssVertex, matchedRange: MatchedRangeWithNode) as this =
    //let weightChanged = Event<Descriptor>()
    let mutable handled = false
    let hashCode =
        let mutable hash = 17
        hash <- hash * 23 + rsmState.GetHashCode()
        hash <- hash * 23 + inputPosition.GetHashCode()
        hash <- hash * 23 + gssVertex.GetHashCode()
        hash
        
    (*do gssVertex.WeightChanged.Add (fun _ -> if not handled then weightChanged.Trigger(this))
       match matchedRange.Node with
       | Some n -> n.WeightChanged.Add(fun _ ->
                            printfn "???"
                            if not handled then weightChanged.Trigger(this))
       | None -> ()
    *)
    //member this.WeightChanged = weightChanged.Publish
    member val IsFinal = false with get, set
    member this.RsmState = rsmState
    member this.InputPosition = inputPosition
    member this.GssVertex = gssVertex
    member this.MatchedRange = matchedRange
    member this.Handled
        with get () = handled
        and set v = handled <- v
    member this.Weight
        with get() =
            let treeWeight =
                match this.MatchedRange.Node with
                | Some n -> n.Weight
                | None -> 0<weight>
            this.GssVertex.MinimalWeightOfLeftPart + treeWeight
    override this.GetHashCode() = hashCode
    override this.Equals (y:obj) =
        y :? Descriptor
        && (y :?> Descriptor).RsmState = this.RsmState
        && (y :?> Descriptor).InputPosition = this.InputPosition
        && (y :?> Descriptor).GssVertex = this.GssVertex

and RsmState (isStart: bool, isFinal: bool) =
    let id = getFirstFreeRsmStateId()
    let errorRecoveryLabels = HashSet()
    let coveredTargetStates = HashSet()    
    let outgoingTerminalEdges = Dictionary<Char, HashSet<RsmState>>()
    let outgoingNonTerminalEdges= Dictionary<RsmState, HashSet<RsmState>>()
    let nonTerminalNodes = ResizeArray<INonTerminalNode>()
    let mutable rsmBox : Option<IRsmBox> = None
    new () = RsmState(false,false)
    
    member this.Id = id
    member this.ErrorRecoveryLabels = errorRecoveryLabels
    member this.OutgoingTerminalEdges = outgoingTerminalEdges
    member this.OutgoingNonTerminalEdges = outgoingNonTerminalEdges    
    member this.IsStart = isStart
    member this.IsFinal = isFinal
    member this.NonTerminalNodes = nonTerminalNodes
    member this.AddTerminalEdge (terminal, targetState) =
        if coveredTargetStates.Contains targetState |> not
        then
            let added = errorRecoveryLabels.Add terminal
            assert added
            let added =  coveredTargetStates.Add targetState
            assert added
        let exists,targetStates = this.OutgoingTerminalEdges.TryGetValue terminal
        if exists
        then
            targetStates.Add targetState |> ignore
        else 
            this.OutgoingTerminalEdges.Add(terminal, HashSet [|targetState|])
    member this.AddNonTerminalEdge (nonTerminal, targetState) =
        let exists,targetStates = (this).OutgoingNonTerminalEdges.TryGetValue nonTerminal
        if exists
        then
            targetStates.Add targetState |> ignore
        else 
            this.OutgoingNonTerminalEdges.Add(nonTerminal, HashSet [|targetState|])    
    member this.Box
        with get () =
                match rsmBox with
                | None -> failwith "Rsm state without rsm box."
                | Some b -> b
        and set v = rsmBox <- Some v

and IRsmBox =
    abstract FinalStates: HashSet<RsmState>
    abstract Nonterminal: INonterminal
   
and LinearInputGraphVertexBase (id:int32) =
    let id = id
    let mutable outgoingEdge : Option<Char * TerminalEdgeTarget> = None
    let descriptors = HashSet<Descriptor>()
    let terminalNodes = Dictionary<LinearInputGraphVertexBase, Dictionary<Char, ITerminalNode>>()
    let nonTerminalNodes = Dictionary<LinearInputGraphVertexBase, Dictionary<RsmState, INonTerminalNode>>()
    let rangeNodes = Dictionary<MatchedRange, IRangeNode>()        
    let intermediateNodes = Dictionary<MatchedRange, Dictionary<MatchedRange, IIntermediateNode>>()
    override this.GetHashCode() = id
    
    member this.AddOutgoingEdge (terminal, target) =
        match outgoingEdge with
        | None -> outgoingEdge <- Some (terminal, target)
        | Some x -> failwithf $"Edge exists: %A{x}"

    member this.Id = id
    member this.OutgoingEdge =
        match outgoingEdge with
        | Some v -> v
        | None -> failwith "Unexpected end of input"
    member this.Descriptors = descriptors
    member this.TerminalNodes = terminalNodes
    member this.NonTerminalNodesStartedHere = nonTerminalNodes
    member this.RangeNodes = rangeNodes
    member this.IntermediateNodes = intermediateNodes
    
and [<Struct>] TerminalEdgeTarget =
    val TargetVertex: LinearInputGraphVertexBase
    val Weight: int<weight>
    new (targetVertex, weight) = {TargetVertex = targetVertex; Weight = weight}
    new (targetVertex) = {TargetVertex = targetVertex; Weight = 0<weight>}

and [<Struct>] Range<'position> =
    val StartPosition: 'position
    val EndPosition: 'position
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

and [<Struct>] MatchedRange =
    val InputRange : Range<LinearInputGraphVertexBase>
    val RSMRange : Range<RsmState>
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
    abstract Weight: int<weight> with get, set
    abstract Parents: ResizeArray<IRangeNode>
and IEpsilonNode =
    abstract Parents: ResizeArray<IRangeNode>
and INonTerminalNode =
    abstract Weight: int<weight> with get, set
    abstract Parents: ResizeArray<IRangeNode>
    abstract RangeNodes: ResizeArray<IRangeNode>
    abstract LeftPosition : LinearInputGraphVertexBase
    abstract RightPosition : LinearInputGraphVertexBase
    abstract NonTerminalStartState : RsmState
and IIntermediateNode =
    abstract Weight: int<weight> with get, set
    abstract Parents: ResizeArray<IRangeNode>
and IRangeNode =
    abstract WeightChanged: IEvent<unit> with get
    abstract Weight: int<weight> with get, set
    abstract Parents: ResizeArray<INonRangeNode>
    abstract IntermediateNodes: HashSet<INonRangeNode>
and IGssEdge =
    abstract RsmState: RsmState
    abstract GssVertex: IGssVertex
    abstract MatchedRange: MatchedRangeWithNode

and IGssVertex =
    //abstract WeightChanged: IEvent<unit> with get
    abstract MinimalWeightOfLeftPart: int<weight> with get, set
    abstract InputPosition: LinearInputGraphVertexBase
    abstract RsmState: RsmState
    abstract OutgoingEdges: ResizeArray<IGssEdge>
    abstract Popped: ResizeArray<MatchedRangeWithNode>
    abstract HandledDescriptors: HashSet<Descriptor>

and INonRangeNode =
    abstract Weight: int<weight>
    abstract Parents: ResizeArray<IRangeNode>

