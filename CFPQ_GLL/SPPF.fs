module CFPQ_GLL.SPPF

open System
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core
open FSharpx.Collections

type [<Measure>] distance
type Distance = Unreachable | Reachable of int

type TerminalNode (terminal: int<terminalSymbol>, graphRange: Range<inputGraphVertex>) =
    let mutable parent = None
    let mutable length = 1<distance>
    member this.Terminal = terminal
    member this.LeftPosition = graphRange.StartPosition
    member this.RightPosition = graphRange.EndPosition
    member this.Range = graphRange
    member this.Parent
        with get () = parent
        and set v = parent <- v
    member this.Length
        with get () = length
        and set v = length <- v

and [<Struct>] EpsilonNode =    
    val Position : int<inputGraphVertex>
    val NonTerminalStartState : int<rsmState>
    new (position, nonTerminalStartState)  =
        {            
            Position = position
            NonTerminalStartState = nonTerminalStartState
        }

and IntermediateNode (rsmState:int<rsmState>
                       , inputPosition:int<inputGraphVertex>
                       , leftSubtree: RangeNode
                       , rightSubtree: RangeNode) =
    let mutable parent = None
    //let mu
    member this.RSMState = rsmState
    member this.InputPosition = inputPosition
    member this.LeftSubtree = leftSubtree
    member this.RightSubtree = rightSubtree
    member this.Parent
        with get () = parent
        and set v = parent <- v

and RangeNode (matchedRange: MatchedRange
               , intermediateNodes: HashSet<NonRangeNode>) =
    member this.InputStartPosition = matchedRange.InputRange.StartPosition
    member this.InputEndPosition = matchedRange.InputRange.EndPosition
    member this.RSMStartPosition = matchedRange.RSMRange.StartPosition
    member this.RSMEndPosition = matchedRange.RSMRange.EndPosition
    member this.IntermediateNodes = intermediateNodes    
  
and [<Struct>] IntermediatePoint =
    val RSMState : int<rsmState>
    val InputPosition : int<inputGraphVertex>
    new (rsmState, inputPosition) = {RSMState = rsmState; InputPosition = inputPosition}

and NonTerminalNode (nonTerminalStartState: int<rsmState>, graphRange: Range<inputGraphVertex>, rangeNodes:ResizeArray<RangeNode>) =
    member this.NonTerminalStartState = nonTerminalStartState
    member this.LeftPosition =graphRange.StartPosition
    member this.RightPosition =graphRange.EndPosition
    member this.RangeNodes =rangeNodes    
                
and [<RequireQualifiedAccess>]NonRangeNode =
    | TerminalNode of TerminalNode
    | NonTerminalNode of NonTerminalNode
    | EpsilonNode of EpsilonNode
    | IntermediateNode of IntermediateNode
    | Empty
    
and [<Struct>] Range<[<Measure>]'position> =
    val StartPosition: int<'position>
    val EndPosition: int<'position>
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}

(*
and [<RequireQualifiedAccess; Struct>] RangeType =    
    | Terminal of terminal:TerminalNode
    | NonTerminal of nonTerminal:NonTerminalNode
    | EpsilonNonTerminal of epsilonNonTerminal:EpsilonNode
    | Intermediate of intermediatePoint: IntermediateNode
    | Empty
  *)  
and [<Struct>] MatchedRange =
    val InputRange : Range<inputGraphVertex>
    val RSMRange : Range<rsmState>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

[<Struct>]
type MatchedRangeWithNode =
    val Range : MatchedRange
    val Node: Option<RangeNode>
    new (range, rangeNode) = {Range = range; Node = Some rangeNode}
    new (inputRange, rsmRange, rangeNode) = {Range = MatchedRange(inputRange, rsmRange); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeNode) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); Node = None}

    
[<Struct>]
[<CustomComparison; StructuralEquality>]
type DistanceInfo =
    val Distance: int
    val AtLeastOneMustHaveStateVisited: bool
    new (distance, atLeastOneMustHaveStateVisited) =
        {
            Distance = distance
            AtLeastOneMustHaveStateVisited = atLeastOneMustHaveStateVisited
        }
           
    interface IComparable with
        member this.CompareTo (y:obj) =
            if y :? DistanceInfo
            then
                let y = y :?> DistanceInfo
                if ((not this.AtLeastOneMustHaveStateVisited) && (not y.AtLeastOneMustHaveStateVisited))
                   || (this.AtLeastOneMustHaveStateVisited && y.AtLeastOneMustHaveStateVisited)
                then y.Distance.CompareTo this.Distance
                elif this.AtLeastOneMustHaveStateVisited
                then 1
                else -1
            else failwith "Incomparable."
    

type MatchedRanges () =
    let rangeNodes : Dictionary<MatchedRange,RangeNode> = Dictionary<_,_>()
    let intermediateNodes = Dictionary<MatchedRange, Dictionary<MatchedRange,IntermediateNode>>()
    let terminalNodes = Dictionary<Range<inputGraphVertex>,Dictionary<int<terminalSymbol>,TerminalNode>>()
    let nonTerminalNodes = Dictionary<Range<inputGraphVertex>,Dictionary<int<rsmState>,NonTerminalNode>>()
    let blockSize = 1000
    
    member internal this.AddTerminalNode (range, terminal) =
        let exists, nodes = terminalNodes.TryGetValue range
        if exists
        then
            let exists, terminalNode = nodes.TryGetValue terminal
            if exists
            then terminalNode
            else
                let newTerminalNode = TerminalNode(terminal,range)
                nodes.Add(terminal, newTerminalNode)
                newTerminalNode
        else
            let newTerminalNode = TerminalNode(terminal,range)
            let d = Dictionary<_,_>()
            d.Add(terminal, newTerminalNode)
            terminalNodes.Add(range, d)
            newTerminalNode
                
    member internal this.AddNonTerminalNode (range:Range<inputGraphVertex>, nonTerminalStartState:int<rsmState>, (query:RSM)) =
        let exists, nodes = nonTerminalNodes.TryGetValue range
        let mkNewNonTerminal () =
            let rangeNodes =
                    let res = ResizeArray()
                    for final in query.GetFinalStatesForBoxWithThisStartState nonTerminalStartState do
                        let matchedRange = MatchedRange (range, Range<rsmState>(nonTerminalStartState, final))
                        let exists, rangeNode = rangeNodes.TryGetValue matchedRange
                        if exists then res.Add rangeNode
                    res
            NonTerminalNode(nonTerminalStartState, range, rangeNodes)
            
        if exists
        then
            let exists, nonTerminalNode = nodes.TryGetValue nonTerminalStartState
            if exists
            then nonTerminalNode
            else
                let newNonTerminalNode = mkNewNonTerminal ()
                nodes.Add(nonTerminalStartState, newNonTerminalNode)
                newNonTerminalNode
        else
            let newNonTerminalNode = mkNewNonTerminal ()
            let d = Dictionary<_,_>()
            d.Add(nonTerminalStartState, newNonTerminalNode)
            nonTerminalNodes.Add(range, d)
            newNonTerminalNode
    
    member internal this.AddToMatchedRange (matchedRange: MatchedRange, node:NonRangeNode) =
        let exists, rangeNode = rangeNodes.TryGetValue matchedRange
        if exists
        then
            rangeNode.IntermediateNodes.Add node |> ignore            
            rangeNode
        else
            let rangeNode = RangeNode(matchedRange, HashSet<_> [|node|])
            rangeNodes.Add(matchedRange, rangeNode)
            rangeNode
                            
    member internal this.AddIntermediateNode (leftSubRange: MatchedRangeWithNode, rightSubRange: MatchedRangeWithNode) =
        match leftSubRange.Node with
        | None -> rightSubRange
        | Some n ->
            let mkNode () =
                IntermediateNode(
                                    leftSubRange.Range.RSMRange.EndPosition
                                    , leftSubRange.Range.InputRange.EndPosition
                                    , leftSubRange.Node.Value
                                    , rightSubRange.Node.Value
                                )
            let intermediateNode =
                let exists, rightPart = intermediateNodes.TryGetValue leftSubRange.Range
                if exists
                then
                    let exists, intermediateNode = rightPart.TryGetValue rightSubRange.Range
                    if exists
                    then intermediateNode
                    else
                        let intermediateNode = mkNode()
                        rightPart.Add(rightSubRange.Range, intermediateNode)
                        intermediateNode
                else
                    let intermediateNode = mkNode()
                    let d = Dictionary<_,_>()
                    d.Add (rightSubRange.Range, intermediateNode)
                    intermediateNodes.Add(leftSubRange.Range, d)
                    intermediateNode
                                        
            let newMatchedRange = MatchedRange (leftSubRange.Range.InputRange.StartPosition
                                        , rightSubRange.Range.InputRange.EndPosition
                                        , leftSubRange.Range.RSMRange.StartPosition
                                        , rightSubRange.Range.RSMRange.EndPosition)
            let rangeNode = this.AddToMatchedRange (newMatchedRange, NonRangeNode.IntermediateNode intermediateNode)
            let newRange = MatchedRangeWithNode(newMatchedRange, rangeNode)
            newRange
         
    member private this.Ranges with get () = rangeNodes
    
    member this.NonTerminalNodesStorage with get () = nonTerminalNodes
    
    member this.NonTerminals nonTerminalStartState =
        [|
            for kvp in nonTerminalNodes do
                for kvp in kvp.Value do
                    if kvp.Key = nonTerminalStartState
                    then yield kvp.Value
        |]
     
    member this.GetShortestDistances (
            query: RSM,
            finalStates:HashSet<int<rsmState>>,
            mustVisitStates:HashSet<int<rsmState>>,
            precomputedDistances:Dictionary<MatchedRange,DistanceInfo>,            
            startVertex,
            finalVertices) =
        ResizeArray<_>()
        (*
        let computedShortestDistances = precomputedDistances
        let cycles = HashSet<_>()
        let rec computeShortestDistance range =            
            let exists, computedDistance = computedShortestDistances.TryGetValue range
            if exists
            then computedDistance
            else
                if cycles.Contains range
                then DistanceInfo (Int32.MaxValue, false)
                else
                    cycles.Add range |> ignore
                    let atLeastMustHaveStateVisited =
                        mustVisitStates.Contains range.RSMRange.StartPosition
                        || mustVisitStates.Contains range.RSMRange.EndPosition
                    let exists,types = rangesToTypes.TryGetValue range
                    if not exists
                    then DistanceInfo (Int32.MaxValue, false)
                    else 
                    let distance =
                        [|
                            for rangeType in types ->
                                match rangeType with
                                | RangeType.Empty -> failwith "Empty range in shortest path."
                                | RangeType.EpsilonNonTerminal _ -> DistanceInfo (0,atLeastMustHaveStateVisited)
                                | RangeType.Terminal _ -> DistanceInfo(1,atLeastMustHaveStateVisited)
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
                                    DistanceInfo (leftRangeDistance.Distance + rightRangeDistance.Distance, atLeastMustHaveStateVisited || leftRangeDistance.AtLeastOneMustHaveStateVisited || rightRangeDistance.AtLeastOneMustHaveStateVisited) 
                        |]
                        |> Array.filter (fun x -> x.Distance >= 0)
                        |> fun a -> if a.Length > 0 then Array.min a else DistanceInfo(Int32.MaxValue, false)
                    computedShortestDistances.Add(range, distance)
                    cycles.Remove range |> ignore
                    distance
        let res = ResizeArray<_>()
        let reachable = HashSet<_>()
        //for startVertex in startVertices do
        for finalVertex in finalVertices do
            for finalState in finalStates do
                MatchedRange(startVertex, finalVertex, query.OriginalStartState, finalState)
                |> computeShortestDistance
                |> fun (distance: DistanceInfo) ->                    
                    res.Add (
                        startVertex,
                        finalVertex,
                        if distance.Distance = Int32.MaxValue || not distance.AtLeastOneMustHaveStateVisited
                        then Unreachable
                        else
                            reachable.Add (startVertex,finalVertex) |> ignore
                            Reachable distance.Distance)
        let res = 
            HashSet res
            |> Seq.filter
                (fun (_from,_to,_d) ->
                    let idReachable = reachable.Contains (_from,_to)  
                    (idReachable && _d <> Unreachable)
                    || (not idReachable)                    
                )
            |> ResizeArray
        res
    *)    
    
[<RequireQualifiedAccess>]
type TriplesStoredSPPFNode =
    | EpsilonNode of int<inputGraphVertex> * int<rsmState>
    | TerminalNode of int<inputGraphVertex> * int<terminalSymbol> * int<inputGraphVertex>
    | NonTerminalNode of int<inputGraphVertex> * int<rsmState> * int<inputGraphVertex>
    | IntermediateNode of int<inputGraphVertex> * int<rsmState>
    | RangeNode of int<inputGraphVertex> * int<inputGraphVertex> * int<rsmState> * int<rsmState> 

type TriplesStoredSPPF<'inputVertex when 'inputVertex: equality> (roots:array<NonTerminalNode>) =
    let mutable nodesCount = 0
    let nodes = Dictionary<_,TriplesStoredSPPFNode>()
    let edges = ResizeArray<_>()
    let visitedRangeNodes = Dictionary<RangeNode,_>()
    let visitedNonTerminalNodes = Dictionary<NonTerminalNode,_>()
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
        if visitedNonTerminalNodes.ContainsKey node
        then addEdge parentId visitedNonTerminalNodes.[node]
        else 
            let currentId = nodesCount
            visitedNonTerminalNodes.Add(node, currentId)
            nodes.Add(currentId, TriplesStoredSPPFNode.NonTerminalNode(node.LeftPosition, node.NonTerminalStartState, node.RightPosition))
            addEdge parentId currentId
            nodesCount <- nodesCount + 1
            node.RangeNodes
            |> ResizeArray.iter (handleRangeNode (Some currentId))
        
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
            |> Seq.iter (handleNonRangeNode (Some currentId))
            
    do  roots |> Array.iter (handleNonTerminalNode None)
    
    let printEdge (x,y) = sprintf $"%i{x}->%i{y}"
    let printNode nodeId node =
        match node with
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to) ->
            sprintf $"%i{nodeId} [label = \"%A{_from}, %i{_terminal}, %A{_to}\", shape = rectangle]"
        | TriplesStoredSPPFNode.IntermediateNode (_inputPos, _rsmState) ->
            sprintf $"%i{nodeId} [label = \"Input: %A{_inputPos}; RSM: %i{_rsmState}\", shape = plain]"
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to) ->
            sprintf $"%i{nodeId} [label = \"%A{_from}, %i{_nonTerminal}, %A{_to}\", shape = invtrapezium]"
        | TriplesStoredSPPFNode.EpsilonNode (_pos, _nonTerminal) ->
            sprintf $"%i{nodeId} [label = \"Input: %A{_pos}; RSM: %i{_nonTerminal}\", shape = invhouse]"
        | TriplesStoredSPPFNode.RangeNode (_inputFrom, _inputTo, _rsmFrom, _rsmTo) ->
            sprintf $"%i{nodeId} [label = \"Input: %A{_inputFrom}, %A{_inputTo}; RSM: %i{_rsmFrom}, %i{_rsmTo}\", shape = ellipse]"
            
    member this.ToDot filePath =
        System.IO.File.WriteAllLines(filePath, ["digraph g {"; yield! (nodes |> Seq.map (fun kvp -> printNode kvp.Key kvp.Value)); yield! (ResizeArray.map printEdge edges); "}"])
    
    member this.Edges with get() = edges
    member this.Nodes with get () = nodes