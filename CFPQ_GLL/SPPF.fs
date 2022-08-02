module CFPQ_GLL.SPPF

open System
open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core
open FSharpx.Collections

type Distance = Unreachable | Reachable of int

type TerminalNode (terminal: int<terminalSymbol>, graphRange: Range<IInputGraphVertex>) =
    let parents = HashSet<IRangeNode>()
    let mutable distance = 1<distance>
    member this.Terminal = terminal
    member this.LeftPosition = graphRange.StartPosition
    member this.RightPosition = graphRange.EndPosition
    member this.Range = graphRange    
    interface ITerminalNode with
        member this.Parents = parents        
        member this.Distance
            with get () = distance
            and set v = distance <- v

and [<Struct>] EpsilonNode =    
    val Position : IInputGraphVertex
    val NonTerminalStartState : IRsmState
    new (position, nonTerminalStartState)  =
        {            
            Position = position
            NonTerminalStartState = nonTerminalStartState
        }
    interface IEpsilonNode

and IntermediateNode (rsmState:IRsmState
                       , inputPosition:IInputGraphVertex
                       , leftSubtree: IRangeNode
                       , rightSubtree: IRangeNode) =
    let parents = HashSet<IRangeNode>()
    let mutable distance = leftSubtree.Distance + rightSubtree.Distance        
    member this.RSMState = rsmState
    member this.InputPosition = inputPosition
    member this.LeftSubtree = leftSubtree
    member this.RightSubtree = rightSubtree    
    interface IIntermediateNode with
        member this.Parents = parents    
        member this.Distance
            with get () = distance
            and set v = distance <- v

and RangeNode (matchedRange: MatchedRange, intermediateNodes: HashSet<NonRangeNode>) =
    let mutable distance =
        let mutable minDistance = Int32.MaxValue * 1<distance>
        for node in intermediateNodes do
            minDistance <- min minDistance node.Distance            
        minDistance
    let parents = HashSet<NonRangeNode>()    
            
    member this.InputStartPosition = matchedRange.InputRange.StartPosition
    member this.InputEndPosition = matchedRange.InputRange.EndPosition
    member this.RSMStartPosition = matchedRange.RSMRange.StartPosition
    member this.RSMEndPosition = matchedRange.RSMRange.EndPosition        
    interface IRangeNode with
        member this.Distance
            with get () = distance
            and set v = distance <- v
        member this.Parents = parents
        member this.IntermediateNodes = intermediateNodes

(*  
and [<Struct>] IntermediatePoint =
    val RSMState : IRsmState
    val InputPosition : int<inputGraphVertex>
    new (rsmState, inputPosition) = {RSMState = rsmState; InputPosition = inputPosition}
*)

and NonTerminalNode (nonTerminalStartState: IRsmState, graphRange: Range<IInputGraphVertex>, rangeNodes:ResizeArray<IRangeNode>) =
    let parents = HashSet<IRangeNode>()
    let mutable distance =
        rangeNodes |> ResizeArray.fold (fun v n -> min v n.Distance) (Int32.MaxValue * 1<distance>) 
    member this.NonTerminalStartState = nonTerminalStartState
    member this.LeftPosition =graphRange.StartPosition
    member this.RightPosition =graphRange.EndPosition
    member this.RangeNodes = rangeNodes    
    interface INonTerminalNode with
        member this.Distance
            with get () = distance
            and set v = distance <- v
        member this.Parents = parents
    

(*and [<Struct>] Range<[<Measure>]'position> =
    val StartPosition: int<'position>
    val EndPosition: int<'position>
    new (startPosition, endPosition) = {StartPosition = startPosition; EndPosition = endPosition}
*)
(*and [<Struct>] MatchedRange =
    val InputRange : Range<inputGraphVertex>
    val RSMRange : Range<rsmState>
    new (inputRange, rsmRange) = {InputRange = inputRange; RSMRange = rsmRange}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {InputRange = Range<_>(inputFrom, inputTo); RSMRange = Range<_>(rsmFrom, rsmTo)}

[<Struct>]
type PartOfMatchedRange<'graphVertex> =
    val InputStartPosition : 'graphVertex
    val RSMRange : Range<rsmState>
    new (matchedRange: MatchedRange) = {InputStartPosition = matchedRange.InputRange.StartPosition; RSMRange = matchedRange.RSMRange}
*)
(*[<Struct>]
type MatchedRangeWithNode =
    val Range : MatchedRange
    val Node: Option<RangeNode>
    new (range, rangeNode) = {Range = range; Node = Some rangeNode}
    new (inputRange, rsmRange, rangeNode) = {Range = MatchedRange(inputRange, rsmRange); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo, rangeNode) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); Node = rangeNode}
    new (inputFrom, inputTo, rsmFrom, rsmTo) = {Range = MatchedRange(inputFrom, inputTo, rsmFrom, rsmTo); Node = None}

*)
(*    
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
*)    

type MatchedRanges () =    
    
    let updateDistances (rangeNode:IRangeNode) =
        let cycle = HashSet<IRangeNode>()
        let rec handleRangeNode (rangeNode:IRangeNode) =
            if not <| cycle.Contains rangeNode
            then
                let added = cycle.Add rangeNode
                assert added
                let oldDistance = rangeNode.Distance 
                let mutable newDistance = Int32.MaxValue * 1<distance>
                for node in rangeNode.IntermediateNodes do
                    newDistance <- min newDistance node.Distance            
                if oldDistance > newDistance 
                then
                    rangeNode.Distance <- newDistance
                    rangeNode.Parents
                    |> Seq.iter handleNonRangeNode
            let removed = cycle.Remove rangeNode
            assert removed
            
        and handleNonRangeNode (node:NonRangeNode) =
            match node with
            | NonRangeNode.TerminalNode t -> failwith "Terminal node can not be parent."
            | NonRangeNode.NonTerminalNode n -> handleNonTerminalNode n
            | NonRangeNode.IntermediateNode i -> handleIntermediateNode i
            | NonRangeNode.EpsilonNode _ -> failwith "Epsilon node can not be parent."
            
        and handleNonTerminalNode (node:INonTerminalNode) =            
            let oldDistance = node.Distance
            let _node = node :?> NonTerminalNode
            let newDistance = _node.RangeNodes |> ResizeArray.fold (fun v n -> min v n.Distance) (Int32.MaxValue * 1<distance>)
            if oldDistance > newDistance
            then
                node.Distance <- newDistance
                node.Parents |> Seq.iter handleRangeNode
        
        and handleIntermediateNode (node:IIntermediateNode) =
            let oldDistance = node.Distance
            let _node = node :?> IntermediateNode
            let newDistance = _node.LeftSubtree.Distance + _node.RightSubtree.Distance 
            if oldDistance > newDistance
            then
                node.Distance <- newDistance
                node.Parents |> Seq.iter handleRangeNode
        
        handleRangeNode rangeNode    
    
    member internal this.AddTerminalNode (range:Range<IInputGraphVertex>, terminal) =
        let terminalNodes = range.EndPosition.TerminalNodes
        let exists, nodes = terminalNodes.TryGetValue range.StartPosition
        if exists
        then
            let exists, terminalNode = nodes.TryGetValue terminal
            if exists
            then terminalNode
            else
                let newTerminalNode = TerminalNode(terminal,range) :> ITerminalNode
                nodes.Add(terminal, newTerminalNode)
                newTerminalNode
        else
            let newTerminalNode = TerminalNode(terminal,range) :> ITerminalNode
            let d = Dictionary<_,_>()
            d.Add(terminal, newTerminalNode)
            terminalNodes.Add(range.StartPosition, d)
            newTerminalNode
                
    member internal this.AddNonTerminalNode (range:Range<IInputGraphVertex>, nonTerminalStartState:IRsmState) =
        let rangeNodes = range.EndPosition.RangeNodes
        let nonTerminalNodes = range.EndPosition.NonTerminalNodes
        let exists, nodes = nonTerminalNodes.TryGetValue range.StartPosition        
        let mkNewNonTerminal () =
            let rangeNodes =
                    let res = ResizeArray()
                    for final in  nonTerminalStartState.Box.FinalStates do
                        let matchedRange = MatchedRange (range, Range<IRsmState>(nonTerminalStartState, final))
                        let exists, rangeNode = rangeNodes.TryGetValue matchedRange
                        if exists then res.Add rangeNode
                    res
            let node = NonTerminalNode(nonTerminalStartState, range, rangeNodes)
            rangeNodes |> ResizeArray.iter (fun n -> n.Parents.Add (NonRangeNode.NonTerminalNode node) |> ignore)
            nonTerminalStartState.NonTerminalNodes.Add node
            node :> INonTerminalNode
            
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
            nonTerminalNodes.Add(range.StartPosition, d)
            newNonTerminalNode
    
    member internal this.AddToMatchedRange (matchedRange: MatchedRange, node:NonRangeNode) =
        let rangeNodes = matchedRange.InputRange.EndPosition.RangeNodes
        let exists, rangeNode = rangeNodes.TryGetValue matchedRange
        if exists
        then
            rangeNode.IntermediateNodes.Add node |> ignore
            node.Parents.Add rangeNode |> ignore

            if node.Distance < rangeNode.Distance
            then updateDistances rangeNode
            
            rangeNode
        else
            let rangeNode = RangeNode(matchedRange, HashSet<_> [|node|])
            rangeNodes.Add(matchedRange, rangeNode)
            rangeNode
                            
    member internal this.AddIntermediateNode (leftSubRange: MatchedRangeWithNode, rightSubRange: MatchedRangeWithNode) =
        match leftSubRange.Node with
        | None -> rightSubRange
        | Some n ->
            let intermediateNodes = rightSubRange.Range.InputRange.EndPosition.IntermediateNodes
            let mkNode () =
                let node = 
                    IntermediateNode(
                        leftSubRange.Range.RSMRange.EndPosition
                        , leftSubRange.Range.InputRange.EndPosition
                        , leftSubRange.Node.Value
                        , rightSubRange.Node.Value
                    )
                let n = NonRangeNode.IntermediateNode node
                leftSubRange.Node.Value.Parents.Add n |> ignore  
                rightSubRange.Node.Value.Parents.Add n |> ignore
                node :> IIntermediateNode
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

    member this.GetShortestDistances (
            //query: RSM,
            //finalStates:HashSet<int<rsmState>>,
            //mustVisitStates:HashSet<int<rsmState>>,
            //precomputedDistances:Dictionary<MatchedRange,DistanceInfo>,            
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

type TriplesStoredSPPF<'inputVertex when 'inputVertex: equality> (roots:array<INonTerminalNode>, vertexMap:Dictionary<IInputGraphVertex,int<inputGraphVertex>>) =
    let rsmStatesMap = Dictionary<IRsmState,int<rsmState>>()
    let mutable firstFreeRsmStateId = 0<rsmState>
    let getStateId state =
        let exists,stateId = rsmStatesMap.TryGetValue state
        if exists
        then stateId
        else
            let stateId = firstFreeRsmStateId
            rsmStatesMap.Add(state,stateId)
            firstFreeRsmStateId <- firstFreeRsmStateId + 1<rsmState>
            stateId
            
    let mutable firstFreeGraphVertexId =
        if vertexMap.Values.Count = 0
        then 0<inputGraphVertex>
        else Seq.max vertexMap.Values + 1<inputGraphVertex>
    let getVertexId vertex =
        let exists,vertexId = vertexMap.TryGetValue vertex
        if exists
        then vertexId
        else
            let vertexId = firstFreeGraphVertexId
            vertexMap.Add(vertex,vertexId)
            firstFreeGraphVertexId <- firstFreeGraphVertexId + 1<inputGraphVertex>
            vertexId
            
    let mutable nodesCount = 0
    let nodes = Dictionary<_,TriplesStoredSPPFNode>()
    let edges = ResizeArray<_>()
    let visitedRangeNodes = Dictionary<IRangeNode,_>()
    let visitedNonTerminalNodes = Dictionary<INonTerminalNode,_>()
    let addEdge parentId currentId =
        match parentId with
        | Some x -> edges.Add(x,currentId)
        | None -> ()
    
    let rec handleIntermediateNode parentId (node:IIntermediateNode) =
        let node = node :?> IntermediateNode
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.IntermediateNode(getVertexId node.InputPosition, getStateId node.RSMState))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
        handleRangeNode (Some currentId) node.LeftSubtree
        handleRangeNode (Some currentId) node.RightSubtree
    
    and handleTerminalNode parentId (node:ITerminalNode) =
        let node = node :?> TerminalNode
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.TerminalNode(getVertexId node.LeftPosition, node.Terminal, getVertexId node.RightPosition))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
        
    and handleEpsilonNode parentId (node:IEpsilonNode) =
        let node = node :?> EpsilonNode 
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.EpsilonNode(getVertexId node.Position, getStateId node.NonTerminalStartState))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
    
    and handleNonTerminalNode parentId (node:INonTerminalNode) =
        if visitedNonTerminalNodes.ContainsKey node
        then addEdge parentId visitedNonTerminalNodes.[node]
        else
            let node = node :?> NonTerminalNode
            let currentId = nodesCount
            visitedNonTerminalNodes.Add(node, currentId)
            nodes.Add(currentId, TriplesStoredSPPFNode.NonTerminalNode(getVertexId node.LeftPosition, getStateId node.NonTerminalStartState, getVertexId node.RightPosition))
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
        
    and handleRangeNode parentId (node:IRangeNode) =
        if visitedRangeNodes.ContainsKey node
        then addEdge parentId visitedRangeNodes.[node]
        else
            let node = node :?> RangeNode
            let currentId = nodesCount
            visitedRangeNodes.Add(node, currentId)
            nodes.Add(currentId, TriplesStoredSPPFNode.RangeNode(getVertexId node.InputStartPosition,
                                                                 getVertexId node.InputEndPosition,
                                                                 getStateId node.RSMStartPosition,
                                                                 getStateId node.RSMEndPosition))
            addEdge parentId currentId
            nodesCount <- nodesCount + 1
            (node :> IRangeNode).IntermediateNodes
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