module CFPQ_GLL.SPPF

open System
open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Microsoft.FSharp.Core
open FSharpx.Collections

type Distance = Unreachable | Reachable of int

type TerminalNode (terminal: Char, graphRange: Range<LinearInputGraphVertexBase>, weight) =
    let parents = ResizeArray<IRangeNode>()
    let mutable weight = weight
    member this.Terminal = terminal
    member this.LeftPosition = graphRange.StartPosition
    member this.RightPosition = graphRange.EndPosition
    member this.Range = graphRange
    interface ITerminalNode with
        member this.Parents = parents
        member this.Weight
            with get () = weight
            and set v = weight <- v

and EpsilonNode (position:LinearInputGraphVertexBase, nonTerminalStartState:RsmState) =
    let parents = ResizeArray<IRangeNode>()
    member this.Position = position
    member this.NonTerminalStartState = nonTerminalStartState

    interface IEpsilonNode with
        member this.Parents = parents

and IntermediateNode (rsmState:RsmState
                       , inputPosition:LinearInputGraphVertexBase
                       , leftSubtree: IRangeNode
                       , rightSubtree: IRangeNode) =
    let parents = ResizeArray<IRangeNode>()
    let mutable weight = leftSubtree.Weight + rightSubtree.Weight
    member this.RSMState = rsmState
    member this.InputPosition = inputPosition
    member this.LeftSubtree = leftSubtree
    member this.RightSubtree = rightSubtree
    interface IIntermediateNode with
        member this.Parents = parents
        member this.Weight
            with get () = weight
            and set v = weight <- v

and RangeNode (matchedRange: MatchedRange, intermediateNodes: HashSet<INonRangeNode>) =
    let mutable weight =
        intermediateNodes |> Seq.fold (fun v n -> min v n.Weight) (Int32.MaxValue * 1<weight>)
    let parents = ResizeArray<INonRangeNode>()

    member this.InputStartPosition = matchedRange.InputRange.StartPosition
    member this.InputEndPosition = matchedRange.InputRange.EndPosition
    member this.RSMStartPosition = matchedRange.RSMRange.StartPosition
    member this.RSMEndPosition = matchedRange.RSMRange.EndPosition
    interface IRangeNode with
        member this.Weight
            with get () = weight
            and set v = weight <- v
        member this.Parents = parents
        member this.IntermediateNodes = intermediateNodes

and NonTerminalNode (nonTerminalStartState: RsmState, graphRange: Range<LinearInputGraphVertexBase>, rangeNodes:ResizeArray<IRangeNode>) =
    let parents = ResizeArray<IRangeNode>()
    let mutable weight =
        let res = rangeNodes |> ResizeArray.fold (fun v n -> min v n.Weight) (Int32.MaxValue * 1<weight>)
        assert (res < Int32.MaxValue * 1<weight>)
        res
    interface INonTerminalNode with
        member this.NonTerminalStartState = nonTerminalStartState
        member this.LeftPosition = graphRange.StartPosition
        member this.RightPosition = graphRange.EndPosition
        member this.RangeNodes = rangeNodes
        member this.Weight
            with get () = weight
            and set v =
                assert (weight < Int32.MaxValue * 1<weight>)
                weight <- v
        member this.Parents = parents

and [<RequireQualifiedAccess>]NonRangeNode =
    | TerminalNode of ITerminalNode
    | NonTerminalNode of INonTerminalNode
    | EpsilonNode of IEpsilonNode
    | IntermediateNode of IIntermediateNode

    interface INonRangeNode with
        member this.Weight =
            match this with
            | NonRangeNode.TerminalNode t -> t.Weight
            | NonRangeNode.NonTerminalNode n -> n.Weight
            | NonRangeNode.IntermediateNode i -> i.Weight
            | NonRangeNode.EpsilonNode e -> 0<weight>
        member this.Parents =
            match this with
            | NonRangeNode.TerminalNode t -> t.Parents
            | NonRangeNode.NonTerminalNode n -> n.Parents
            | NonRangeNode.IntermediateNode i -> i.Parents
            | NonRangeNode.EpsilonNode e -> e.Parents

type MatchedRanges () =
    static member private updateWeights (rangeNode:IRangeNode) =
        let cycle = HashSet<IRangeNode>()
        let rec handleRangeNode (rangeNode:IRangeNode) =
            if not <| cycle.Contains rangeNode
            then
                let added = cycle.Add rangeNode
                assert added
                let oldWeight = rangeNode.Weight
                let mutable newWeight = Int32.MaxValue * 1<weight>
                for node in rangeNode.IntermediateNodes do
                    newWeight <- min newWeight node.Weight
                if oldWeight > newWeight
                then
                    rangeNode.Weight <- newWeight
                    rangeNode.Parents
                    |> Seq.iter handleNonRangeNode
                let removed = cycle.Remove rangeNode
                assert removed

        and handleNonRangeNode (node:INonRangeNode) =
            match (node :?> NonRangeNode) with
            | NonRangeNode.TerminalNode _ -> failwith "Terminal node can not be parent."
            | NonRangeNode.NonTerminalNode n -> handleNonTerminalNode n
            | NonRangeNode.IntermediateNode i -> handleIntermediateNode i
            | NonRangeNode.EpsilonNode _ -> failwith "Epsilon node can not be parent."

        and handleNonTerminalNode (node:INonTerminalNode) =
            let oldWeight = node.Weight
            let newWeight = node.RangeNodes |> ResizeArray.fold (fun v n -> min v n.Weight) (Int32.MaxValue * 1<weight>)
            if oldWeight > newWeight
            then
                node.Weight <- newWeight
                node.Parents |> Seq.iter handleRangeNode

        and handleIntermediateNode (node:IIntermediateNode) =
            let oldWeight = node.Weight
            let _node = node :?> IntermediateNode
            let newWeight = _node.LeftSubtree.Weight + _node.RightSubtree.Weight
            if oldWeight > newWeight
            then
                node.Weight <- newWeight
                node.Parents |> Seq.iter handleRangeNode

        handleRangeNode rangeNode

    // member internal this.AddTerminalNode (range:Range<ILinearInputGraphVertex>, terminal, distance) =
    //     let terminalNodes = range.EndPosition.TerminalNodes
    //     let exists, nodes = terminalNodes.TryGetValue range.StartPosition
    //     assert ((not exists) || (exists && nodes.Count = 1))
    //     if exists
    //     then
    //         let terminalNode = nodes.Values |> Seq.head
    //         if terminalNode.Distance > distance
    //         then
    //             let newTerminalNode = TerminalNode(terminal,range,distance) :> ITerminalNode
    //             nodes.Clear()
    //             nodes.Add(terminal, newTerminalNode)
    //             terminalNode.Parents |> Seq.iter (newTerminalNode.Parents.Add >> ignore)
    //             terminalNode.Parents |> Seq.iter MatchedRanges.updateDistances
    //             newTerminalNode
    //         else
    //             terminalNode
    //     else
    //         let newTerminalNode = TerminalNode(terminal,range,distance) :> ITerminalNode
    //         let d = Dictionary<_,_>()
    //         d.Add(terminal, newTerminalNode)
    //         terminalNodes.Add(range.StartPosition, d)
    //         newTerminalNode

    // Original
        // member internal this.AddTerminalNode (range:Range<ILinearInputGraphVertex>, terminal, distance) =
        // let terminalNodes = range.EndPosition.TerminalNodes
        // let exists, nodes = terminalNodes.TryGetValue range.StartPosition
        // if exists
        // then
        //     let exists, terminalNode = nodes.TryGetValue terminal
        //     if exists
        //     then
        //         if terminalNode.Distance > distance
        //         then
        //             terminalNode.Distance <- distance
        //             terminalNode.Parents |> Seq.iter MatchedRanges.updateDistances
        //         terminalNode
        //     else
        //         let newTerminalNode = TerminalNode(terminal,range,distance) :> ITerminalNode
        //         nodes.Add(terminal, newTerminalNode)
        //         newTerminalNode
        // else
        //     let newTerminalNode = TerminalNode(terminal,range,distance) :> ITerminalNode
        //     let d = Dictionary<_,_>()
        //     d.Add(terminal, newTerminalNode)
        //     terminalNodes.Add(range.StartPosition, d)
        //     newTerminalNode

    member internal this.AddTerminalNode (range:Range<LinearInputGraphVertexBase>, terminal, weight) =
        if terminal = Epsilon
        then ()
        let terminalNodes = range.EndPosition.TerminalNodes
        let exists, nodes = terminalNodes.TryGetValue range.StartPosition
        let newNode =
            if exists
            then
                let exists, terminalNode = nodes.TryGetValue terminal
                if exists then terminalNode
                else
                    let newTerminalNode = TerminalNode(terminal,range,weight) :> ITerminalNode
                    nodes.Add(terminal, newTerminalNode)
                    newTerminalNode
            else
                let newTerminalNode = TerminalNode(terminal,range,weight) :> ITerminalNode
                let d = Dictionary<_,_>()
                d.Add(terminal, newTerminalNode)
                terminalNodes.Add(range.StartPosition, d)
                newTerminalNode

        newNode

    member internal this.AddNonTerminalNode (range:Range<LinearInputGraphVertexBase>, nonTerminalStartState:RsmState) =
        let rangeNodes = range.EndPosition.RangeNodes
        let nonTerminalNodes = range.StartPosition.NonTerminalNodesStartedHere
        let exists, nodes = nonTerminalNodes.TryGetValue range.EndPosition
        let mkNewNonTerminal () =
            let rangeNodes = // TODO: check
                    let res = ResizeArray()
                    let mutable curMinWeight = Int32.MaxValue * 1<weight>
                    for final in nonTerminalStartState.Box.FinalStates do
                        let matchedRange = MatchedRange (range, Range<_>(nonTerminalStartState, final))
                        let exists, rangeNode = rangeNodes.TryGetValue matchedRange
                        if exists
                        then
                            res.Add rangeNode
                            if rangeNode.Weight < curMinWeight
                            then curMinWeight <- rangeNode.Weight
                    res |> ResizeArray.filter (fun n -> n.Weight = curMinWeight)

            let node = NonTerminalNode(nonTerminalStartState, range, rangeNodes)
            rangeNodes |> ResizeArray.iter (fun n -> n.Parents.Add (NonRangeNode.NonTerminalNode node))
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
            nonTerminalNodes.Add(range.EndPosition, d)
            newNonTerminalNode

    member internal this.AddToMatchedRange (matchedRange: MatchedRange, node:INonRangeNode) =
        let rangeNodes = matchedRange.InputRange.EndPosition.RangeNodes
        let exists, rangeNode = rangeNodes.TryGetValue matchedRange
        let node =
            if exists
            then
                if node.Weight <= rangeNode.Weight then
                    rangeNode.IntermediateNodes.Add node |> ignore
                    node.Parents.Add rangeNode

                    if node.Weight < rangeNode.Weight
                    then
                        let removed = rangeNode.IntermediateNodes.RemoveWhere (fun x -> x.Weight > node.Weight)
                        if removed = 0 then printfn $"%A{node.Weight} < %A{rangeNode.Weight}"
                        assert (removed > 0)
                        MatchedRanges.updateWeights rangeNode
                rangeNode
            else
                let rangeNode = RangeNode(matchedRange, HashSet<_> [|node|])
                node.Parents.Add rangeNode                                                   
                rangeNodes.Add(matchedRange, rangeNode)
                rangeNode
        node

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
                leftSubRange.Node.Value.Parents.Add n
                rightSubRange.Node.Value.Parents.Add n
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

    static member Invalidate (node:ITerminalNode) =
        let rec handleTerminalNode (terminalNode:ITerminalNode) =
            terminalNode.Parents
            |> Seq.iter (fun node ->
                let removed = node.IntermediateNodes.Remove (NonRangeNode.TerminalNode terminalNode)
                assert removed
                handleRangeNode node
                )

        and handleRangeNode (rangeNode: IRangeNode) =
            if rangeNode.IntermediateNodes.Count = 0
            then
                rangeNode.Parents
                |> Seq.iter (fun node -> handleNonRangeNode (node :?> NonRangeNode))
            else MatchedRanges.updateWeights rangeNode

        and handleNonRangeNode (nonRangeNode : NonRangeNode) =
            match nonRangeNode with
            | NonRangeNode.IntermediateNode i -> handleIntermediateNode i
            | NonRangeNode.NonTerminalNode n -> handleNonTerminalNode n
            | NonRangeNode.EpsilonNode _ -> failwith "Epsilon node can not be parent."
            | NonRangeNode.TerminalNode _ -> failwith "Terminal node can not be parent."

        and handleIntermediateNode (intermediateNode: IIntermediateNode) =
            intermediateNode.Parents
            |> Seq.iter (fun node ->
                let removed = node.IntermediateNodes.Remove (NonRangeNode.IntermediateNode intermediateNode)
                assert removed
                handleRangeNode node)

        and handleNonTerminalNode (nonTerminalNode: INonTerminalNode) =
            if nonTerminalNode.RangeNodes.Count = 0
            then
                nonTerminalNode.LeftPosition.NonTerminalNodesStartedHere.[nonTerminalNode.RightPosition].Remove nonTerminalNode.NonTerminalStartState
                nonTerminalNode.Parents
                |> Seq.iter (fun node -> handleRangeNode node)
            else
                let oldWeight = nonTerminalNode.Weight
                let newWeight = nonTerminalNode.RangeNodes |> ResizeArray.fold (fun v n -> min v n.Weight) (Int32.MaxValue * 1<weight>)
                if oldWeight > newWeight
                then
                    node.Weight <- newWeight
                    nonTerminalNode.Parents
                    |> Seq.iter (fun node -> MatchedRanges.updateWeights node)

        handleTerminalNode node


[<RequireQualifiedAccess>]
type TriplesStoredSPPFNode =
    | EpsilonNode of int<inputGraphVertex> * INonterminal * int<weight>
    | TerminalNode of int<inputGraphVertex> * Char * int<inputGraphVertex> * int<weight>
    | NonTerminalNode of int<inputGraphVertex> * INonterminal * int<inputGraphVertex> * int<weight>
    | IntermediateNode of int<inputGraphVertex> * int<rsmState> * int<weight>
    | RangeNode of int<inputGraphVertex> * int<inputGraphVertex> * int<rsmState> * int<rsmState>

type TriplesStoredSPPF<'inputVertex when 'inputVertex: equality> (roots:array<INonTerminalNode>, vertexMap:Dictionary<LinearInputGraphVertexBase,int<inputGraphVertex>>) =
    let rsmStatesMap = Dictionary<RsmState,int<rsmState>>()
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
        nodes.Add(currentId, TriplesStoredSPPFNode.IntermediateNode(getVertexId node.InputPosition, getStateId node.RSMState, (node :> IIntermediateNode).Weight))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1
        handleRangeNode (Some currentId) node.LeftSubtree
        handleRangeNode (Some currentId) node.RightSubtree

    and handleTerminalNode parentId (node:ITerminalNode) =
        let node = node :?> TerminalNode
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.TerminalNode(getVertexId node.LeftPosition, node.Terminal, getVertexId node.RightPosition, (node :> ITerminalNode).Weight))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1

    and handleEpsilonNode parentId (node:IEpsilonNode) =
        let node = node :?> EpsilonNode
        let currentId = nodesCount
        nodes.Add(currentId, TriplesStoredSPPFNode.EpsilonNode(getVertexId node.Position, node.NonTerminalStartState.Box.Nonterminal, 0<weight>))
        addEdge parentId currentId
        nodesCount <- nodesCount + 1

    and handleNonTerminalNode parentId (node:INonTerminalNode) =
        if visitedNonTerminalNodes.ContainsKey node
        then addEdge parentId visitedNonTerminalNodes.[node]
        else
            let currentId = nodesCount
            visitedNonTerminalNodes.Add(node, currentId)
            nodes.Add(currentId, TriplesStoredSPPFNode.NonTerminalNode(getVertexId node.LeftPosition, node.NonTerminalStartState.Box.Nonterminal, getVertexId node.RightPosition, node.Weight))
            addEdge parentId currentId
            nodesCount <- nodesCount + 1
            node.RangeNodes
            |> ResizeArray.iter (handleRangeNode (Some currentId))

    and handleNonRangeNode parentId (node: INonRangeNode) =
        match (node :?> NonRangeNode) with
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
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to, w) ->
            sprintf $"%i{nodeId} [label = \"%A{_from}, {string _terminal}, %A{_to}, Weight: {w}\", shape = rectangle]"
        | TriplesStoredSPPFNode.IntermediateNode (_inputPos, _rsmState, w) ->
            sprintf $"%i{nodeId} [label = \"Interm Input: %A{_inputPos}; RSM: %i{_rsmState}, Weight: {w}\", shape = plain]"
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to, w) ->            
            sprintf $"%i{nodeId} [label = \"%A{_from}, {_nonTerminal.Name}, %A{_to}, Weight: {w}\", shape = invtrapezium]"
        | TriplesStoredSPPFNode.EpsilonNode (_pos, _nonTerminal, w) ->                        
            sprintf $"%i{nodeId} [label = \"EpsNode Input: %A{_pos}; RSM: {_nonTerminal.Name}, Weight: {w}\", shape = invhouse]"
        | TriplesStoredSPPFNode.RangeNode (_inputFrom, _inputTo, _rsmFrom, _rsmTo) ->
            sprintf $"%i{nodeId} [label = \"RangeNode Input: %A{_inputFrom}, %A{_inputTo}; RSM: %i{_rsmFrom}, %i{_rsmTo}\", shape = ellipse]"

    member this.ToDot filePath =        
        System.IO.File.WriteAllLines(filePath, ["digraph g {"; yield! (nodes |> Seq.map (fun kvp -> printNode kvp.Key kvp.Value)); yield! (ResizeArray.map printEdge edges); "}"])

    member this.Edges with get() = edges
    member this.Nodes with get () = nodes
